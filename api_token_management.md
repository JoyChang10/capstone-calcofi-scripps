# API Token Management
This document explains how to set up the credentials required to run the CalCOFI data pipeline.

## Service Account Key
We use a Google Cloud Service Account for non-interactive authentication (no browser login required).
- The key is a JSON file.
- **DO NOT** commit this JSON file to GitHub.
- Store the key in the `/secrets` folder of this project.

## Environment Configuration
To keep our keys secure, we use a `.Renviron` file. 
1. Create a file named `.Renviron` in the project root.
2. Add the following line, replacing the path with the location of your JSON key:
   `GDRIVE_AUTH_PATH="secrets/your-key-filename.json"`
3. Restart your R session for changes to take effect.

## Security Requirements
- The `.gitignore` file must include `.Renviron` and `*.json`.
- Never share the JSON key via Slack or email; use a secure password manager or direct transfer if a teammate needs it.

## Google Drive Access
For the pipeline to work, the Google Drive folder must be shared with the Service Account email:
- **Email:** calcofi-data-pipeline-bot@project-660fc419-8133-4635-a41.iam.gserviceaccount.com
- **Permission:** Editor.