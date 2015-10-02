Welcome to myDiss!
===================

This is repository contains code to:

 - **process** raw data 
 - subequently **analyse** it 
 - **output** graphs and tables
 - **typeset** results and text into a final document

Structure
-------------
 The folder structure in the repositiory follows a typical approach I use for organizing a project. Raw data, and and all steps and resources necessary for processeing (i.e. scripts / functions / packages) are kept seperately.

Explore Git
-------------

We'll check out one of the folders within the repository and see what Git can do for us. Navigate to  the folder <i class="icon-folder-open"></i> [Diss/Analysis/source/][1] 

You will see a list of files, with some comments that might seem random (e.g. "Calculated ratios, added CI + plotting" for the first file "HC_plot.R"), and a date. Have a look around for some abstract, but tidy code. I recommend the script "data_subsetting_plot.R" as an example of good coding practice.

The comment and date refer to the last time a particular file was changed, and when. However, the comment is often not specific or unique to a file, and looking more closely, you will find that many files share the same comment. 

This comes from the way Git manages version control:

> After you're happy with changes you made to one, or multiple files, you **commit** these changes to your online repository, and this is how you keep track of what you (or others) have been doing.

**Note:* Have a look at git's [*features*][5] and how the [*online file storage*][6] may look like.

Commits
-------------
Let's go back to <i class="icon-folder-open"></i> [Diss/][2] and check out some of the latest commits. Above the list of files and folders, click on [*Commits*][3] - there should be 59 of them. 

Here you can see an overview of all the changes that I committed. Each commit comes with a title, a comment, the author's name and a date. Other options (e.g. browsing a snapshot of a repository from the past) are available on the right hand side.

Scroll down a bit, and click on the commit called "[*TS Graphs, initial BMD table *][4]" from the 23rd of July, 2015. You'll see which files I modified, and how - added lines are shown in green, and changed, or deleted ones in red. 

The version control system is what makes git so powerful:
> Even after overwriting a file with a new save, you can always go back to a previous commit and restore what was lost.


  [1]: https://github.com/the-Hull/Diss/tree/master/Analysis/source
  [2]: https://github.com/the-Hull/Diss/
  [3]: https://github.com/the-Hull/Diss/commits/master
  [4]: https://github.com/the-Hull/Diss/commit/2688c07591fc8e8099ca7db71d3a0f5955674477
  [5]: https://github.com/features
  [6]: https://www.youtube.com/watch?v=uLR1RNqJ1Mw
