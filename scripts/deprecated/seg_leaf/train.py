import argparse
import tensorflow as tf
import numpy as np
from data_loader import load_data, load_single_image
from model import unet_model

def train_model(images_dir, masks_dir):
    # Load data
    images, masks = load_data(images_dir, masks_dir)

    # Build model
    model = unet_model((128, 128, 3))
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

    # Train the model
    model.fit(images, masks, batch_size=16, epochs=50, validation_split=0.2)

    # Save the model
    model.save("leaf_segmentation_model.h5")

def predict(model_path, image_path):
    model = tf.keras.models.load_model(model_path)
    img = load_single_image(image_path)

    prediction = model.predict(img)
    prediction_mask = (prediction[0] > 0.5).astype(np.uint8) * 255  # Binarize

    # Save the prediction mask as an image (optional)
    from tensorflow.keras.preprocessing.image import save_img
    save_img("prediction_mask.png", prediction_mask)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Train or Predict using U-Net model.")
    parser.add_argument('--train', action='store_true', help="Train the model.")
    parser.add_argument('--images', type=str, help="Directory of images for training.")
    parser.add_argument('--masks', type=str, help="Directory of masks for training.")
    parser.add_argument('--predict', action='store_true', help="Run inference on a single image.")
    parser.add_argument('--model', type=str, help="Path to the trained model.")
    parser.add_argument('--input', type=str, help="Path to the input image for prediction.")

    args = parser.parse_args()

    if args.train:
        if args.images and args.masks:
            train_model(args.images, args.masks)
        else:
            print("Please provide both --images and --masks directories for training.")
    
    if args.predict:
        if args.model and args.input:
            predict(args.model, args.input)
        else:
            print("Please provide both --model path and --input image for prediction.")
