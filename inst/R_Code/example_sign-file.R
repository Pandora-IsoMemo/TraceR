library(openssl)

# Generate RSA keys
key <- rsa_keygen(bits = 2048)  # Generates a key pair with 2048 bits
private_key <- key
public_key <- as.list(key)$pubkey
# Optionally save keys to files
write_pem(private_key, "private_key.pem")
write_pem(public_key, "public_key.pem")

library(jsonlite)
# Example data
data <- list(name = "John Doe", age = 30, valid = TRUE)
# Convert to JSON string
json_data <- toJSON(data, pretty = TRUE, auto_unbox = TRUE)

# Create a signature
signature <- signature_create(data = charToRaw(json_data), key = key)

# Encode the signature in base64 for easier handling
encoded_signature <- base64_encode(signature)

# Append the signature to the original data
signed_data <- list(data = data, signature = encoded_signature)

# Convert the complete structure back to JSON
signed_json_data <- toJSON(signed_data, pretty = TRUE, auto_unbox = TRUE)

# Save the signed JSON to a file
writeLines(signed_json_data, "signed_data.json")

# Load the signed JSON
received_data <- fromJSON("signed_data.json")
received_json_data <- toJSON(received_data$data, pretty = TRUE, auto_unbox = TRUE)
received_signature <- base64_decode(received_data$signature)

# Verify the signature
tryCatch({
  is_valid <- signature_verify(data = charToRaw(received_json_data), sig = received_signature, pubkey = public_key)
  message(paste("Verification result:", is_valid))  # Will print TRUE if valid, FALSE otherwise
  is_valid
}, error = function(e) {
  message(paste("Error during verification:", e$message))
  return(FALSE)
})
