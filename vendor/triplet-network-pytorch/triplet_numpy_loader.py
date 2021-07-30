import numpy as np
import torch
import torch.utils.data
import torchvision.transforms as transforms

class TripletNumpyLoader(torch.utils.data.Dataset):        
    def __init__(self, features_filename, triplets_filename, **kwargs):
        np_feat = np.lib.format.read_array(open(features_filename, 'rb'))
        np_feat = np.reshape(np_feat, [-1, 1, 30, 30])
        self.features = torch.from_numpy(np_feat).float()
        self.triplets = torch.from_numpy(np.lib.format.read_array(open(triplets_filename, 'rb'))).long()

    def __getitem__(self, index):
        x, y, z = self.triplets[index]
        return self.features[x], self.features[y], self.features[z]

    def __len__(self):
        return len(self.triplets)
