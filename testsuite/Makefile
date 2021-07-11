default:

test = all

glox:
	cd ../glox && make build
	dart tool/bin/test.dart $(test) -i ../glox/target/glox

rlox:
	cd ../rlox && cargo build --release
	dart tool/bin/test.dart $(test) -i ../rlox/target/release/rlox
