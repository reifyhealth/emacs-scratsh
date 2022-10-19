# Scratsh

## Purpose

Scratsh provides scratch buffer functionality to shell buffers in Emacs. Example shell buffers include: `ansi-term` and `vterm`.

In brief: Scratsh = **scrat**ch + **sh**ell

## Installation

For now, place `scratsh.el` somewhere under the `load-path`.

## Configuration

Configuration variables are stored under the `scratsh` group and can be set by executing `customize-group` and selecting `scratsh`.

## Use

<To be written. For now, see the "public api" section of `scratsh.el`.>

## Motivation

The "command prompt" is a poor tool for editing expressions past a certain size. A better kind of tool already exists in the form of scratch buffers, such as Emacs' `*scratch*` buffer and Cider's `*cider-scratch*` buffer. Before Scratsh, there was no mechanism to use scratch buffers with shells such as `bash` in a fully functional terminal emulator such as `ansi-term` or `vterm`. The closest approximation to a proper scratch buffer is to 1) set a buffer to `sh-mode`, 2) open a shell with `sh-show-shell`, and then 3) use `sh-execute-region` or `sh-send-line-or-region-and-step`.  but this workflow does not support key workflows such as: 1) opening a scratch buffer for an already existing shell, and 2) using commands such as `less` which require a "fully functional terminal".

## Concepts

This program considers two types of buffers: the *shell buffer* and the *scratch buffer*.

A **scratch buffer** is an Emacs buffer which is used to **edit** text and to **send** text to a shell. Text in a scratch buffer is edited as per usual in Emacs. Text sent to a shell using the various`scratch-send-*` commands. The paradigm case of a scratch buffer is the built-in Emacs `*scratch*` buffer, which is used to edit Elisp commands.

A **shell buffer** is an Emacs buffer which is used for I/O for some subprocess. The paradigm case of a shell buffer is Emacs' `ansi-term` style of buffers.

A shell buffer may have at most one **primary** scratch buffer. The primary scratch buffer is only special in that it can be easily switched to using `scratsh-switch`.

A scratch buffer may be **connected** to exactly one shell buffer. When a scratch buffer is connected to a shell buffer `B`, then the various `scratch-send-*` commands will send text to the `B` for evaluation. Note that multiple scratch buffers can be connected to a common shell buffer.


