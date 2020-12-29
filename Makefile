ubuntu-build:
	cd infra/Ubuntu/build && chmod +x image_build.sh && ./image_build.sh

ubuntu-push:
	cd infra/Ubuntu/build && chmod +x image_push.sh && ./image_push.sh
