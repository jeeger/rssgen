# rssgen #

rssgen generates a RSS feed from `git log` output and writes the
resulting RSS file to a specified location. Following the Unix
philosophy, it is written as a filter (albeit with file output being
done internally).

My website is generated from `org-mode` markup and exported as HTML,
and rssgen is written to allow me to add an RSS feed to my
website. I've made rssgen as configurable as possible.

## How it works ##

rssgen parses the output of `git log`. To be more precise, the output
of `git log --pretty='%x1f%H%x1f%an%x1f%aD%x1f%s%x1f%b%x1f' --numstat
-z`, as this seems to be the easiest way to get the information
required by the script.

The log is then transformed into an RSS feed. Information such as
author, title, description and publication date are taken directly
from the log output.

To generate links to the correct pages, we first find the file that's
most likely to contain the interesting changes. We do this by finding
the file with the most additions, or if nothing has been added, the
file with the most deletions. If neither is available, just link to
the front page.

I'll see how well this algorithm works, and potentially change it.

Not all entries in the log are included in the RSS feed, only those
where the description contains the string "\n\nINCLUDE_RSS\n". This
allows me to exclude smaller changes from the feed.


## Configuration options ##

rssgen is configured through the `rssgen.conf` file. The following
configuration are available:

 - filesource: The prefix to remove from the source file to get the link.
 - filedest: The prefix to add to the source file to get the link.
 - fromext: File extension of the source files.
 - toext: File extension of the destination file.
 - rssfile: Location of the generated RSS file.
 - rssdesc: A description for the RSS feed.
 - rsstitle: A title for the full RSS feed.
 - rsslink: Link to the site's front page.
 - copyright: A copyright statement.
