import numpy as np
import os
from tensorflow.keras.preprocessing.image import load_img, img_to_array

def load_data(image_dir, mask_dir=None):
    images = []
    masks = []

    for img_name in os.listdir(image_dir):
        img = load_img(os.path.join(image_dir, img_name), target_size=(128, 128))
        img = img_to_array(img) / 255.0  # Normalize to [0, 1]

        if mask_dir:  # Load masks if mask_dir is provided
            mask_name = img_name.split('.')[0] + '_mask.png'
            mask = load_img(os.path.join(mask_dir, mask_name), target_size=(128, 128), color_mode='grayscale')
            mask = img_to_array(mask) / 255.0  # Normalize to [0, 1]
            masks.append(mask)

        images.append(img)

    return np.array(images), np.array(masks) if masks else None
