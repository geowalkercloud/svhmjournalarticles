library(qrcode)

qr <- qr_code("https://geowalkercloud.github.io/svhmjournalarticles/")

# Save to PNG
png("svhm_icu_publications_qr.png", width = 600, height = 600)
plot(qr)
dev.off()
