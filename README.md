# Emacs 配置

## company-mode 和 company-clang

### 使用`company-mode`和`company-clang`自动补全，配置如下：

```lisp
(use-package cc-mode)

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

;; add .dir-locals.el to project root
;; ((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
;;                                      "-I/home/<user>/project_root/include2/")))))
;; company-clang do company-complete
(use-package company-clang
  :bind(:map c-mode-map
             ("<tab>" . company-complete)
             :map c++-mode-map
             ("<tab>" . company-complete))
  :config
  (setq company-backends (delete 'company-semantic company-backends))

  )
```

> `company-clang`: use `clang` to retrieve completion candidates. You will have completion with system header files, but not your project. By default, `company-complete` already includes `company-clang` backend, so you can use `company-complete` to complete for many thing. Note that in the configuration for `company-mode` above, we have to delete `company-semantic`, otherwise `company-complete` will use `company-semantic` instead of `company-clang`, because it has higher precedence in `company-backends`. 


### `company-clang`补全当前工程
如果需要`company-clang`补全当前路径下的`symbol`，必须告诉`Clang`当前工程的`include`文件夹。

> To retrieve completion candidates for your projects, you will have to tell Clang where your include paths are. Create a file named `.dir-locals.el` at your project root:

```lisp
((nil . ((company-clang-arguments . ("-I/home/<user>/project_root/include1/"
                                     "-I/home/<user>/project_root/include2/")))))
```



## company-c-headers

1. `company-c-headers`的配置

```lisp
(use-package company-c-headers
  :defer 2
  :ensure t
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/")
  )
```



2. 系统路径的设置

```lisp
  (add-to-list 'company-c-headers-path-system "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/")
```

用命令`g++ -v -x c++ -E -`可以得到系统头文件的目录。



3. 用户的`include`路径的设置。设置后，用户`include`的目录也可以被`company-mode`识别并补全(`complete`)。

> After that, you can complete C++ header files. To complete project local, use `company-c-headers-path-user` and put it in`.dir-locals.el`.

```lisp
(add-to-list 'company-c-headers-path-user "/Users/zhangli/Desktop/DeepAR_Algorithm/include/")
```

也可以在用户当前工程的根目录下建立`.dir-locals.el`文件，并在文件里面设置。

```lisp
(
 (nil . ((company-c-headers-path-user . ("/Users/zhangli/Desktop/DeepAR_Algorithm/include/"))))
 )
```

## semantic mode

### 头文件补全

1. 添加`system include` 路径

```lisp
(semantic-add-system-include "/usr/include/boost" 'c++-mode)
;; c++-mode and c-mode
(semantic-add-system-include "~/linux/kernel")
```

⚠️`semantic-dependency-system-include-path`只是在`local buffer`有值，global 为 `nil`

>  You can view the list of include paths in `semantic-dependency-system-include-path`. 

⚠️下面这段还没有得到实际的验证

> `semantic-add-system-include` will add your include paths to `semantic-c-dependency-system-include-path` variable.

### 代码补全

1. `company-semantic`

>  Semantic parses source code and creates a database for code completion

在配置`company-clang`时， 把`company-semantic`从`company-backends`中删除了，如果没有删除`company-semantic`:

> `company-mode` provides a command called `company-semantic` that uses SemanticDB to retrieve completion candidates. Function interface of each candidate is shown in the minibuffer. 

> One nice thing of `company-semantic` is that it fixed an issue of original Semantic completion `semantic-ia-complete-symbol`: it can show you completions even if there's no prefix. The original `semantic-ia-complete-symbol` requires to have at least one character as a prefix for finding completions.


## senator

> Senator stands for SEmantic NAvigaTOR,  Senator is a part of CEDET

1. `senator-go-to-up-reference`
   可以从方法跳转到属于该方法类的定义

> move up one reference level from current tag. An upper reference level of a tag is the source that defines the tag or includes the tag. This is incredibly useful when you want to jump from a function declaration in a class to its definition, or jump to the class that a function belongs to


## function-args

- moo-jump-local

## (Optional) Project management with EDE

### `ede` 配置

```lisp

(use-package ede
  :config
  (message "hello ede")
  ;; ede-mode part of cedet to manage project
  (require 'ede)
  (global-ede-mode 1)
  (ede-cpp-root-project "deepar"
                        :file "~/Desktop/DeepAR_Algorithm/Makefile"
                        :include-path '("/include")
                        :system-include-path '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/")
                        )
  )
```

- The first argument to `ede-cpp-root-project` is project name. 

- `:file` argument specifies path to project root. You must create a file in the project root, since EDE uses that file as an "anchor" to project root; in our case, `Makefile` is the anchor file. 

- `:include-path` specifies directories local to your projects that EDE should search first when looking for a header file. `:include-path` is relative to project root specified in :file.

- :`system-include-path` specifies system include paths that do not belong to current project. Note that despite it is called `system-include-path`, it does not have to be in place like `/usr/include`. You can specify any include directories outside of your current project as "system headers".

**注意⚠️**
> However, you have to do one last thing: either close the file `main.c` and reopen it or `M-x semantic-force-refresh` to tell Semantic to analyze `main.c` again. Otherwise, Semantic will still keep the previous parsing result of this file and completion candidates won't be available. As a result, it is important to load EDE projects defined by `ede-cpp-root-project` before opening any project file.

## Semantic Stickyfunc Enhance
> This package is an improvement of stock `semantic-stickyfunc-mode` in `Semantic`, a parser framework bundled with Emacs. `semantic-stickyfunc-mode` shows function interface/class/namespace that has part of it at the top of current visible screen. 

```lisp
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)
(require 'stickyfunc-enhance)
```

## ggtags 和 helm-gtags
配置和`keybinding`

```lisp
(use-package ggtags
  :ensure t
  :init
    (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode 1))))
  :bind (:map ggtags-mode-map
              ("C-c g s" . ggtags-find-other-symbol)
              ("C-c g h" . ggtags-view-tag-history)
              ("C-c g r" . ggtags-find-reference)
              ("C-c g f" . ggtags-find-file)
              ("C-c g c" . ggtags-create-tags)
              ("C-c g u" . ggtags-update-tags)
              )
  :config

  )

(use-package helm-gtags
  :ensure t
  :commands (helm-gtags-mode)
  :init
    ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
              ("C-c g a" .  helm-gtags-tags-in-this-function)
              ("C-j" .  helm-gtags-select)
              ("C-s-." .  helm-gtags-dwim)
              ("C-s-," .  helm-gtags-pop-stack)
              ("C-c <" .  helm-gtags-previous-history)
              ("C-c >" .  helm-gtags-next-history)
              ("M-," .  pop-tag-mark)
              )
  :config
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   )
)
```
