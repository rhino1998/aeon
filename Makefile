.PHONY: aeon
aeon:
	go build ./cmd/aeon

.PHONY:
test:
	go test -v ./...
