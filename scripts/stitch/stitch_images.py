import cv2
import os
import sys
import argparse
from glob import glob

def load_images_from_folder(folder):
    extensions = ('*.jpg', '*.jpeg', '*.png', '*.bmp', '*.tiff')
    image_files = []
    for ext in extensions:
        image_files.extend(glob(os.path.join(folder, ext)))
    image_files.sort()  # Ensure consistent order
    images = [cv2.imread(f) for f in image_files]
    return images, image_files

def resize_images(images, max_dim=2000):
    resized = []
    for i, img in enumerate(images):
        height, width = img.shape[:2]
        scale = min(max_dim / height, max_dim / width, 1.0)
        new_size = (int(width * scale), int(height * scale))
        resized_img = cv2.resize(img, new_size)
        resized.append(resized_img)
        # Save resized images in same directory (or customize)
        # update filename if needed
    return resized

def main(input_folder, output_path, save_resized_folder=None, max_dim=2000):
    images, image_files = load_images_from_folder(input_folder)
    if len(images) < 2:
        print("Need at least two images to stitch.")
        sys.exit()

    # Resize images to avoid size overflow, keep aspect ratio
    resized_images = resize_images(images, max_dim)

    # Save resized images if directory provided
    if save_resized_folder:
        os.makedirs(save_resized_folder, exist_ok=True)
        for orig_file, res_img in zip(image_files, resized_images):
            basename = os.path.basename(orig_file)
            save_path = os.path.join(save_resized_folder, basename)
            cv2.imwrite(save_path, res_img)
        print(f"Resized images saved to {save_resized_folder}")

    print("Stitching images...")
    stitcher = cv2.Stitcher_create(cv2.STITCHER_SCANS)
    print("Stitching images with 'scans' mode...")
    status, stitched = stitcher.stitch(resized_images)

    if status == cv2.STITCHER_OK:
        cv2.imwrite(output_path, stitched)
        print(f"Stitched image saved to {output_path}")
    else:
        print(f"Error during stitching. Status code: {status}")

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Resize and stitch images in a folder.')
    parser.add_argument('input_folder', help='Folder containing images to process')
    parser.add_argument('output', help='Path to save stitched output image')
    parser.add_argument('--resized_folder', default=None, help='Folder to save resized images (optional)')
    parser.add_argument('--max_dim', type=int, default=2000, help='Maximum dimension for resized images')
    args = parser.parse_args()

    main(args.input_folder, args.output, args.resized_folder, args.max_dim)
