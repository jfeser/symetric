import argparse
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import random_split
from torch.optim import lr_scheduler
import torch.optim as optim

from datasets import BalancedBatchSampler, NumpyDataset
from losses import OnlineTripletLoss
# Strategies for selecting triplets within a minibatch
from utils import AllTripletSelector, HardestNegativeTripletSelector, RandomNegativeTripletSelector, SemihardNegativeTripletSelector 
from metrics import AccumulatedAccuracyMetric, AverageNonzeroTripletsMetric
from trainer import fit

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
        self.conv1 = nn.Conv2d(1, 10, kernel_size=5)
        self.conv2 = nn.Conv2d(10, 20, kernel_size=5)
        self.conv2_drop = nn.Dropout2d()
        self.fc1 = nn.Linear(320, 50)
        self.fc2 = nn.Linear(50, 10)

    def forward(self, x):
        x = F.relu(F.max_pool2d(self.conv1(x), 2))
        x = F.relu(F.max_pool2d(self.conv2_drop(self.conv2(x)), 2))
        x = x.view(-1, 320)
        x = F.relu(self.fc1(x))
        x = F.dropout(x, training=self.training)
        return self.fc2(x)
    
def main():
    args = parser.parse_args()
    args.cuda = not args.no_cuda and torch.cuda.is_available()
    if args.cuda:
        torch.cuda.manual_seed(args.seed)

    dataset = NumpyDataset(args.features, args.labels)
    train_len = int(len(dataset) * 0.8)
    test_len = len(dataset) - train_len
    train_dataset, test_dataset = random_split(dataset, [train_len, test_len])

    train_batch_sampler = BalancedBatchSampler(train_dataset, n_classes=10, n_samples=25)
    test_batch_sampler = BalancedBatchSampler(test_dataset, n_classes=10, n_samples=25)

    kwargs = {'num_workers': 1, 'pin_memory': True} if args.cuda else {}

    online_train_loader = torch.utils.data.DataLoader(train_dataset, batch_sampler=train_batch_sampler, **kwargs)
    online_test_loader = torch.utils.data.DataLoader(test_dataset, batch_sampler=test_batch_sampler, **kwargs)

    margin = args.margin
    embedding_net = EmbeddingNet()
    model = embedding_net
    if args.cuda:
        model.cuda()
        
    loss_fn = OnlineTripletLoss(margin, RandomNegativeTripletSelector(margin))
    optimizer = optim.Adam(model.parameters(), lr=args.lr, weight_decay=1e-4)
    scheduler = lr_scheduler.StepLR(optimizer, 8, gamma=0.1, last_epoch=-1)
    log_interval = 50

    fit(online_train_loader,
        online_test_loader,
        model,
        loss_fn,
        optimizer,
        scheduler,
        args.epochs,
        args.cuda,
        log_interval,
        metrics=[AverageNonzeroTripletsMetric()])

if __name__ == '__main__':
    main()
