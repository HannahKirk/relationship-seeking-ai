#!/bin/bash
# =============================================================================
# Chat Application Launcher
# =============================================================================
#
# Four chat interfaces for testing steering vectors:
#
# 1. chat_cli.py              - Command-line interface (no browser needed)
# 2. chat_app-splitscreen.py  - Two panels: compare different multipliers
# 3. chat_app-multiscreen.py  - Multiple panels: compare different layers
# 4. chat_app-configurable.py - Flexible: any combination of configs
#
# After starting a web app, open: http://localhost:8000
#
# Usage: Run from the tools/ directory
#   cd 2-steering-vector-training/tools
#   python chat_cli.py ...
#
# =============================================================================

# -----------------------------------------------------------------------------
# 1. CLI Chat (terminal only, no web UI)
# -----------------------------------------------------------------------------
# Best for: Quick testing over SSH without port forwarding
# Change multiplier on-the-fly with: "Hello! || 1.5"

# python chat_cli.py --model_name meta-llama/Llama-3.1-8B-Instruct --behavior relationship-seeking --layer 14
