import argparse

import numpy as np
from pytorch_metric_learning import losses, miners, distances, reducers, testers
from pytorch_metric_learning.utils.accuracy_calculator import AccuracyCalculator
import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import random_split


# Training settings
parser = argparse.ArgumentParser(description='')
parser.add_argument('--batch-size', type=int, default=64, metavar='N',
                    help='input batch size for training (default: 64)')
parser.add_argument('--test-batch-size', type=int, default=1000, metavar='N',
                    help='input batch size for testing (default: 1000)')
parser.add_argument('--epochs', type=int, default=10, metavar='N',
                    help='number of epochs to train (default: 10)')
parser.add_argument('--lr', type=float, default=1e-3, metavar='LR',
                    help='learning rate (default: 1e-3)')
parser.add_argument('--momentum', type=float, default=0.5, metavar='M',
                    help='SGD momentum (default: 0.5)')
parser.add_argument('--no-cuda', action='store_true', default=False,
                    help='enables CUDA training')
parser.add_argument('--seed', type=int, default=1, metavar='S',
                    help='random seed (default: 1)')
parser.add_argument('--log-interval', type=int, default=20, metavar='N',
                    help='how many batches to wait before logging training status')
parser.add_argument('--margin', type=float, default=1.0, metavar='M',
                    help='margin for triplet loss (default: 1.0)')
parser.add_argument('--resume', default='', type=str,
                    help='path to latest checkpoint (default: none)')
parser.add_argument('--name', default='TripletNet', type=str,
                    help='name of experiment')
parser.add_argument('--features', type=str, metavar='F')
parser.add_argument('--labels', type=str, metavar='F')

class EmbeddingNet(nn.Module):
    def __init__(self):
        super(EmbeddingNet, self).__init__()
        self.conv1 = nn.Conv2d(1, 32, 3, 1)
        self.conv2 = nn.Conv2d(32, 64, 3, 1)
        self.dropout1 = nn.Dropout(0.25)
        self.dropout2 = nn.Dropout(0.5)
        self.fc1 = nn.Linear(10816, 128)
        self.fc2 = nn.Linear(128, 10)

    def forward(self, x):
        x = self.conv1(x)
        x = F.relu(x)
        x = self.conv2(x)
        x = F.relu(x)
        x = F.max_pool2d(x, 2)
        x = self.dropout1(x)
        x = torch.flatten(x, 1)
        x = self.fc1(x)
        x = F.relu(x)
        x = self.dropout2(x)
        x = self.fc2(x)
        output = F.log_softmax(x, dim=1)
        return output
    
class NumpyDataset(torch.utils.data.Dataset):
    def __init__(self, features_file, labels_file):
        np_feat = np.lib.format.read_array(open(features_file, 'rb'))
        np_feat = np.reshape(np_feat, [-1, 1, 30, 30])
        self.features = torch.from_numpy(np_feat).float()

        np_labels = np.lib.format.read_array(open(labels_file, 'rb'))
        self.labels = torch.from_numpy(np_labels).long()

    def __len__(self):
        return len(self.labels)

    def __getitem__(self, i):
        return self.features[i], self.labels[i]
    

def train(model, loss_func, mining_func, device, train_loader, optimizer, epoch):
    model.train()
    for batch_idx, (data, labels) in enumerate(train_loader):
        data, labels = data.to(device), labels.to(device)
        optimizer.zero_grad()
        embeddings = model(data)
        indices_tuple = mining_func(embeddings, labels)
        loss = loss_func(embeddings, labels, indices_tuple)
        loss.backward()
        optimizer.step()
        if batch_idx % 20 == 0:
            print("Epoch {} Iteration {}: Loss = {}, Number of mined triplets = {}".format(epoch, batch_idx, loss, mining_func.num_triplets))

def get_all_embeddings(dataset, model):
    tester = testers.BaseTester()
    return tester.get_all_embeddings(dataset, model)


def test(train_set, test_set, model, accuracy_calculator):
    train_embeddings, train_labels = get_all_embeddings(train_set, model)
    test_embeddings, test_labels = get_all_embeddings(test_set, model)
    train_labels = train_labels.squeeze(1)
    test_labels = test_labels.squeeze(1)
    print("Computing accuracy")
    accuracies = accuracy_calculator.get_accuracy(test_embeddings, 
                                                  train_embeddings,
                                                  test_labels,
                                                  train_labels,
                                                  False)
    print("Test set accuracy (Precision@1) = {}".format(accuracies["precision_at_1"]))
    print("Test set accuracy (NMI) = {}".format(accuracies["NMI"]))
    return accuracies


def checkpoint(model):
    """Saves checkpoint to disk"""
    dummy_input = torch.randn(1, 1, 30, 30, device='cuda')
    model.eval()
    traced = torch.jit.trace(model, dummy_input)
    traced.save('encoder_best.pt.tar')

    
def main():
    args = parser.parse_args()
    args.cuda = not args.no_cuda and torch.cuda.is_available()
    if args.cuda:
        torch.cuda.manual_seed(args.seed)
        device = torch.device("cuda")

    dataset = NumpyDataset(args.features, args.labels)
    train_len = int(len(dataset) * 0.8)
    test_len = len(dataset) - train_len
    train_dataset, test_dataset = random_split(dataset, [train_len, test_len])

    train_loader = torch.utils.data.DataLoader(train_dataset, batch_size=256, shuffle=True)
    test_loader = torch.utils.data.DataLoader(test_dataset, batch_size=256)

    model = EmbeddingNet()
    if args.cuda:
        model.cuda()
    
    optimizer = optim.Adam(model.parameters(), lr=args.lr, weight_decay=1e-4)
    miner = miners.TripletMarginMiner(type_of_triplets='semihard')
    loss_func = losses.MultiSimilarityLoss()
    accuracy_calculator = AccuracyCalculator(include = ("precision_at_1","NMI"), k = 1)

    best_acc = 0
    for epoch in range(1, args.epochs + 1):
        train(model, loss_func, miner, device, train_loader, optimizer, epoch)
        acc = test(train_dataset, test_dataset, model, accuracy_calculator)['precision_at_1']
        if acc > best_acc:
            best_acc = acc
            checkpoint(model)

if __name__ == '__main__':
    main()
