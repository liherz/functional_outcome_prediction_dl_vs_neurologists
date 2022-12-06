import tensorflow as tf
import numpy as np
from scipy import ndimage

# zoom
@tf.function
def zoom(volume):
    """Zoom the volume by a few degrees"""
    
    volume_shape = volume.shape
    def augment_volume(volume):
        min_zoom = 0.8
        max_zoom = 1.2
        z = np.random.sample() *(max_zoom-min_zoom) + min_zoom
        zoom_matrix = np.array([[z, 0, 0, 0],
                                [0, z, 0, 0],
                                [0, 0, z, 0],
                                [0, 0, 0, 1]])
        return ndimage.affine_transform(volume, zoom_matrix, mode = "nearest", order = 1)
    
    augmented_volume = tf.py_function(augment_volume, [volume], tf.float32)
    augmented_volume.set_shape(volume_shape)
    return augmented_volume


@tf.function
def rotate(volume):
    """Rotate the volume by a few degrees"""
    
    volume_shape = volume.shape
    def augment_volume(volume):
        # define some rotation angles
        angles = [-20, -10, -5, 5, 10, 20]
        # pick angles at random
        angle = np.random.choice(angles)
        # rotate volume
        volume = ndimage.rotate(volume, angle, axes = (0,1), reshape = False, order = 3, mode = "nearest")
        #volume[volume < 0] = 0
        #volume[volume > 1] = 1
        return volume

    augmented_volume = tf.py_function(augment_volume, [volume], tf.float32)
    augmented_volume.set_shape(volume_shape)
    return augmented_volume


@tf.function
def shift(volume):
    """Shift the volume along the x and y axes"""
    
    volume_shape = volume.shape
    def augment_volume(volume):
        # define some shifts in the three different directions
        x_shift = np.random.uniform(-20, 20)
        y_shift = np.random.uniform(-20, 20)
        z_shift = np.random.uniform(0, 0)
        # shift volume
        volume = ndimage.shift(volume, [x_shift, y_shift, z_shift, 0], mode = "nearest", order = 0)
        return volume

    augmented_volume = tf.py_function(augment_volume, [volume], tf.float32)
    augmented_volume.set_shape(volume_shape)
    return augmented_volume


@tf.function
def flip(volume):
    """Randomly flip the volume"""
    volume_shape = volume.shape
    def augment_volume(volume):
        axis = np.random.choice([0,1])
        if(axis == 0): # vertical flip
            volume = volume[:,::-1,:,:]
        return volume

    augmented_volume = tf.py_function(augment_volume, [volume], tf.float32)
    augmented_volume.set_shape(volume_shape)
    return augmented_volume