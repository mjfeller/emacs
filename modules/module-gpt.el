;;; module-gpt.el --- Configuration -*- lexical-binding: t; -*-

;; Author: Mark Feller <mark.feller@member.fsf.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This module configures agent-shell for interacting with AI agents.
;; Provides integration with Claude Code, Cursor, Gemini, and other AI assistants.

;;; Code:

(use-package shell-maker
  :demand t
  :custom
  (shell-maker-transcript-default-path "~/.cache/emacs/"))

(use-package acp
  :demand t)

(use-package agent-shell
  :after (shell-maker acp)
  :demand t
  :custom
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t))
  
  ;; Set Claude Code as the default agent - skips selection prompt
  (agent-shell-preferred-agent-config
   (agent-shell-anthropic-make-claude-code-config))
  ;; Set to nil to be prompted for agent selection on each new shell
  ;; Available agents include: Claude Code, Cursor, Gemini CLI, Goose, Codex, etc.
  
  ;; List of all available agent configurations
  ;; Default includes: Claude Code, Cursor, Gemini, Goose, Codex, OpenCode, Qwen
  (agent-shell-agent-configs (agent-shell--make-default-agent-configs))
  
  ;; Header style: 'graphical (with icon), 'text (simple), or nil (no header)
  (agent-shell-header-style 'graphical)
  (agent-shell-show-welcome-message t)
  (agent-shell-show-config-icons t)
  
  ;; Icons for various UI elements (use SF Symbols on macOS if desired)
  (agent-shell-permission-icon "⚠")
  (agent-shell-thought-process-icon "💡")
  
  (agent-shell-display-action '(display-buffer-in-side-window (side . right) (window-width . 0.4)))
  ;; Alternative options:
  ;; '(display-buffer-same-window)
  ;; '(display-buffer-pop-up-window)
  ;; '(display-buffer-in-side-window (side . right) (window-width . 0.5))
  ;; '(display-buffer-below-selected (window-height . 0.3))
  
  ;; Enable agent to read/write text files directly
  (agent-shell-text-file-capabilities t)
  
  ;; Enable automatic file path completion in prompts
  (agent-shell-file-completion-enabled t)
  
  ;; Maximum file size for embedding with ContentBlock::Resource (100KB default)
  ;; Files larger than this use ContentBlock::ResourceLink instead
  (agent-shell-embed-file-size-limit 102400)
  
  ;; Highlight source code blocks (off by default for performance)
  ;; See: https://github.com/xenodium/agent-shell/issues/119
  (agent-shell-highlight-blocks nil)
  
  (agent-shell-screenshot-command
   (cond
    ((eq system-type 'darwin) '("/usr/sbin/screencapture" "-i"))
    ((eq system-type 'gnu/linux) '("/usr/bin/import"))
    (t '("import")))) ; ImageMagick fallback
  
  (agent-shell-mcp-servers nil)
  ;; Example configurations:
  ;;
  ;; Stdio transport (universally supported):
  ;; (agent-shell-mcp-servers
  ;;  '(((name . "filesystem")
  ;;     (command . "npx")
  ;;     (args . ("-y" "@modelcontextprotocol/server-filesystem" "/tmp"))
  ;;     (env . ()))
  ;;    ((name . "git")
  ;;     (command . "npx")
  ;;     (args . ("-y" "@modelcontextprotocol/server-git"))
  ;;     (env . ()))))
  ;;
  ;; HTTP transport (requires mcpCapabilities.http):
  ;; (agent-shell-mcp-servers
  ;;  '(((name . "notion")
  ;;     (type . "http")
  ;;     (url . "https://mcp.notion.com/mcp")
  ;;     (headers . (((name . "Authorization")
  ;;                  (value . "Bearer YOUR_TOKEN")))))))
  ;;
  ;; SSE transport (requires mcpCapabilities.sse):
  ;; (agent-shell-mcp-servers
  ;;  '(((name . "my-sse-server")
  ;;     (type . "sse")
  ;;     (url . "https://example.com/mcp")
  ;;     (headers . ()))))
  
  (agent-shell-anthropic-claude-command '("claude-code-acp"))
  (agent-shell-anthropic-claude-environment
   ;; Set XDG_CONFIG_HOME and XDG_DATA_HOME to use cache directory
   ;; This prevents Claude Code from creating directories in ~/.config/emacs
   (list (concat "XDG_CONFIG_HOME=" (expand-file-name "~/.cache/emacs/claude-config"))
         (concat "XDG_DATA_HOME=" (expand-file-name "~/.cache/emacs/claude-data"))))
  
  :bind (("C-c a" . agent-shell)           ; Start/reuse agent shell
         ("C-c A" . agent-shell-anthropic-start-claude-code) ; Start Claude Code
         ("C-c n" . agent-shell-new-shell) ; Force new shell with selection
         ("C-c t" . agent-shell-toggle))   ; Toggle shell visibility
  
  ;; Additional useful commands (bind as needed):
  ;; - agent-shell-interrupt: Interrupt in-progress request (C-c C-c in shell)
  ;; - agent-shell-version: Show agent-shell version
  ;; - agent-shell-set-session-mode: Change session mode
  ;; - agent-shell-cycle-session-mode: Cycle through session modes (C-TAB in shell)
  ;; - agent-shell-next-item: Navigate to next item (TAB or 'n' in shell)
  ;; - agent-shell-previous-item: Navigate to previous item (S-TAB or 'p' in shell)
  
  :config
  (when (called-interactively-p 'any)
    (agent-shell-version))
  
  (when (featurep 'evil)
    (evil-set-initial-state 'agent-shell-mode 'emacs)))

(provide 'module-gpt)
