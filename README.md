# tags
### _Bibek Panthi <bpanthi977 at gmail dot com>_

## Overview
This project allows assigning tags to files & directories and allows navigating the tags directly through filesystem.
This implies, the navigation through tags doesn't required any program and is independent of platform.

## How to use
1. Say you want to assign tags to files & directories within a directory (e.g. Documents)
2. Create a directory `$tags` in the Documents directory 
3. now whenever you add or remove tags (more on how to do this later) to any file/directory within the Documents directory or its subdirectories, appropriate links are created in the `$tags` directory and a record of the tags assigned are stored in `$tags/.tags` file
4. you can later navigate through the links created in `$tags` directory using any filebrower 

## Strucutre of `$tags` directory 
I think this is best explained with examples. 

1. say, you add `physics` tag to the file `Documents/Science/General Relativity - Sean Carroll.pdf` then a link to that file is created in `$tags/physics` directory,
2. when you add tags `programming, maths` to `Documents/Engineering/Numerical Methods.pdf` then a link to that file is created in `$tags/programming/maths` & `$tags/maths/programming` directory,
3. and if you add `civil/materials` tag to `Documents/Civil Engineering/Self healing materials.pdf` file,  a link in `$tags/civil/materials` directory is created.

This strucutre of `$tags` allows browsing for files that satisfy multiple tags without introducting any additional program and UI, by just depending on the filesystem . If you want to find a file that has both tags `physics` and `maths` then goto `$tags/physics/maths` or `$tags/maths/physics`. There, you may see the file you were looking for or upon seeing other subfolders you recall exactly what you want. If you want to look at files that only have `physics` tag, they will be waiting you at `$tags/physics`. If you want to look at all files that have tag `physics` just do `cd \$tags/physics; find .` in the terminal or use your file manager's features to see all the links within the `physics` directory (and its subdirectories). 

## Adding/Removing/Changing tags of a file/directory
This project doesn't provide any UI to do this changes. Lets first look at the API that this project provides, then we'll look at one ui that I use.


1. `(tags:existing-tags #p"/path/to/a/file")` returns the tags assigned to the file (say for example "civil/materials,physics")
2. `(tags:update-tags #p"/path/to/a/file" "newtag1,newtag2")` updates the tags (adding and removing as necessary) to the file and creates/updates the required directory structure in `$tags` directory

These function read the `$tags/.tags` file, look at existing links and do changes as necessary. To cache `.tags` in memory for batch operation on multiple files, use `tags:existing-tags*` and `tags:update-tags*` function enclosed inside the `with-$tag-directory` macro.

For example (assuming some mechanism for UI `abcd:show-ui` exists):

```lisp
(with-$tag-directory ((find-tags-directory file) :read-tags t)
    (abcd:show-ui
        (existing-tags file)
        :onchange (lambda (newtags)
                      (update-tags file newtags))))
``` 

### Example UI for adding/removing tags
For personal uses, I have a CL process that runs in background and does some routine jobs (like checking for notices in the campus website, among other things). It listens to commands on a socket. For example to login into my campus network, I send "login" into the socket through a script and it does the login task.

Now for the tags system, I added following code to my existing system:

```lisp
(defun tags (args)
  (log:info "TAGS: " args)
  (let* ((path (string-trim " " (subseq args (position #\Space args))))
         (file (pathname path)))
    (tags:with-$tag-directory ((tags:find-tags-directory file) :read-tags t)
      (let* ((existing-tags (tags:existing-tags file))
             (new-tags (services:get-input-zenity "Update Tags" "Tags" existing-tags)))
        (when new-tags
          (tags:update-tags* file new-tags))))))

(services:add-command (lambda (str)
                        (uiop:string-prefix-p "tags " str))
                      'tags)
```

Then I added a keyboard shortcut to my filemanger that sends the command "tags /path/to/the/file" through the socket and the above snippet opens a zenity dialog box showing existing tags where I can edit the tags and apply changes. 

## License

MIT License

