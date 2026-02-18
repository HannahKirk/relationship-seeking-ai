#!/bin/bash
# Upload benchmark logs to S3 (internal use)
# Requires: AWS CLI configured, AISI_PLATFORM_BUCKET environment variable set

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
aws s3 sync "$SCRIPT_DIR/logs/" "s3://$AISI_PLATFORM_BUCKET/users/$USER/inspect_logs/" --delete
