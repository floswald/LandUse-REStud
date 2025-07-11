#!/bin/bash

# Validate Stata license file
if [ ! -f /usr/local/stata/stata.lic ]; then
    echo "❌ ERROR: Stata license file not found at /usr/local/stata/stata.lic"
    echo ""
    echo "Please ensure:"
    echo "1. STATA_LICENSE_PATH environment variable is set in your local environment"
    echo "2. The path points to a valid Stata license file"
    echo "3. Example: export STATA_LICENSE_PATH=\"/Users/floswald/Dropbox/licenses/stata.lic\""
    echo ""
    echo "Current mount status:"
    ls -la /usr/local/stata/ || echo "Directory /usr/local/stata/ does not exist"
    exit 1
fi

echo "✅ Stata license file found successfully"

# Continue with regular setup
if [ -f "/usr/local/bin/setup-packages.sh" ]; then
    echo "🔧 Running setup-packages.sh..."
    /usr/local/bin/setup-packages.sh

    echo "🎉 Container setup completed successfully!"
else
    echo "⚠️  setup-packages.sh not found, skipping package setup"
    echo " done"
fi

