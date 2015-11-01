;; User keymap
;; -----------------------------
;; Keymaps are stored as a set of diffs that are merged together to create
;; the final set of keys. You can modify these diffs to either add or
;; subtract bindings.
;;
;; Like behaviors, keys are bound by tag. When objects with those tags are active
;; the key bindings are live. Keys can be bound to any number of Light Table commands,
;; allowing you the flexibility to execute multiple operations together. To see a list
;; of all the commands you can execute, start typing a word related to the thing you
;; want to do in between the square brackets (e.g. type "editor").

[
 [:editor "alt-w" :editor.watch.watch-selection]
 [:editor "alt-shift-w" :editor.watch.unwatch]

 ;; cclaudiu
 [:editor "ctrl-x" :editor.cut]
 [:editor "ctrl-c" :editor.copy]
 [:editor "ctrl-v" :editor.paste]
 [:editor "ctrl-a" :editor.select-all]
 [:editor "ctrl-d" :editor.delete-line]
 [:editor "ctrl-l" :editor.select-line]
 [:editor "ctrl-j" :editor.sublime.joinLines]
 [:editor "ctrl-/" :comment-selection]
 [:editor "ctrl-\\" :uncomment-selection]
 [:editor "ctrl-shift-c" :clear-console]
 [:editor "ctrl-shift-d" :clear-inline-results]
 [:editor "ctrl-shift-t" :tabs.move-new-tabset]
 [:editor "ctrl-tab" :tabset.next]
 [:editor "ctrl-." :editor.jump-to-definition-at-cursor]

 [:app "ctrl-shift--" :window.zoom-in]
 [:app ":w!" :vim-save]
 [:app ":q!" :vim-quit]
 [:app "ctrl-s" :save]
 [:app "ctrl-shift-k" :console.show]
 [:app "ctrl-shift-h" :console.hide]
 [:app "ctrl-shift-l" :goto-line]
 [:app "ctrl-f" :find.show]
 [:app "ctrl-alt-l" :instarepl.toggle-live]
 [:app "ctrl-shift-w" :workspace.show]
 [:app "ctrl-shift-r" :workspace.show-recents]

 [:editor "ctrl-shift-g" :gitlight-status-toggle]
 [:editor "ctrl-alt-a" :lt.plugins.gitlight.git/git-add]
 [:editor "ctrl-alt-c" :lt.plugins.gitlight.git/git-commit]


 [:editor "ctrl-shift-m" :lt.plugins.cljrefactor.function/extract-fn]
 ;; To subtract a binding, prefix the key with '-'  e.g.
 ;;  [:app "-ctrl-shift-d" :docs.search.show]
]
