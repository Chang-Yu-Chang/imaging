import tensorflow as tf

def unet_model(input_shape):
    inputs = tf.keras.layers.Input(input_shape)
    c1 = tf.keras.layers.Conv2D(64, (3, 3), activation='relu', padding='same')(inputs)
    c1 = tf.keras.layers.Conv2D(64, (3, 3), activation='relu', padding='same')(c1)
    p1 = tf.keras.layers.MaxPooling2D((2, 2))(c1)

    c2 = tf.keras.layers.Conv2D(128, (3, 3), activation='relu', padding='same')(p1)
    c2 = tf.keras.layers.Conv2D(128, (3, 3), activation='relu', padding='same')(c2)
    p2 = tf.keras.layers.MaxPooling2D((2, 2))(c2)

    # Additional layers can be defined for deeper U-Net here

    outputs = tf.keras.layers.Conv2D(1, (1, 1), activation='sigmoid')(c2)  # Adjust based on architecture
    model = tf.keras.models.Model(inputs=[inputs], outputs=[outputs])
    return model
