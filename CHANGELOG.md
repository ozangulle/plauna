# Changelog

All notable changes to this project will be documented in this file.

## [2025-05-26.0] - 2025-05-26

### 🚀 Features

- Use IMAP copy and delete when move is not available
- Move e-mail when its category is changed by user
- *(ui)* Make ui better and mobile friendly
- *(ui)* Add the new plauna logo

### 🐛 Bug Fixes

- Change text sanitization for cleaner training texts
- Adjust text sanitization for cleaner training texts
- Moving emails no longer triggers a search through the whole folder
- *(imap client)* Wrong method call during reconnection
- Faulty partial update of connection data on reconnect
- Setting category to n/a now moves messages back to Inbox
- *(ui)* Toast messages cannot be closed anymore

### ⚙️ Miscellaneous Tasks

- Add flow-storm for better debugging experience
- Update ring dependencies

## [2025-03-22.0] - 2025-03-22

### 🚀 Features

- Health check interval for IMAP client watcher is configurable
- Show sanitized text next to the original on the email details page
- *(ui)* Add pie charts to statistics pages for better data overview
- Add optional config parameters for the email client

### 🐛 Bug Fixes

- Choose correct text content to train on when text attachments present
- Evict preferences cache after updating a value
- Throw an exception if no config file can be found during startup

### 📚 Documentation

- Fix the link to the Docker image in README
- Add 'features' and 'screenshots' subsections

### 🎨 Styling

- Remove the delete buttons from admin ui

### ⚙️ Miscellaneous Tasks

- Update JRE 23 Docker image

## [2025-02-23.0] - 2025-02-23

### 🚀 Features

- *(ui)* Remove links to half-baked features

## [2025-02-21.0] - 2025-02-21

### 🚀 Features

- *(ui)* Reorganize e-mail lists and data training
- *(ui)* Clean up and visually improve /emails
- *(ui)* Unify input styles on different pages
- *(ui)* Email details page is styled in the new fashion
- [**breaking**] Remove check for training binaries before starting imap client
- Enrich e-mails parsed from an mbox
- Rename the page "watchers" to "connections"
- Create directories on imap servers upon category creation
- Set log level from ui
- *(ui)* Async operations and errors are shown to the user as toast messages

### 🐛 Bug Fixes

- Message ids are now url encoded in the email list
- Exception when moving emails if they could not be categorized
- Save language metadata in detail view and preferences
- Display category as n/a in email views if note set
- N/a no longer listed as a language in admin
- Event loops restart when they fail
- Change wrong order of functions on main
- Compare categorization threshold with the probability correctly
- Restart all event loops after a failure or restart in messaging
- Add new languages to language preferences with the value false
- Confidence is set properly after categorization

