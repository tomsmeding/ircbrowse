# ircbrowse

IRC Browse is a web service for browsing IRC logs. The
[IRC Browse site is here](https://ircbrowse.tomsmeding.com/).

IRC Browse was originally built and maintained by
[Chris Done](https://github.com/chrisdone), but this fork is now maintained(?)
by [Tom Smeding](https://github.com/tomsmeding).

## Adding a channel

Add to
[`Ircbrowse.Types.Import`](https://github.com/chrisdone/ircbrowse/edit/master/src/Ircbrowse/Types/Import.hs)
and open a pull request.

## Building & running

Build:

    $ stack install

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser ircbrowse -P'
    $ sudo su postgres --command 'createdb ircbrowse -O ircbrowse'

Update the database to the latest migration:

    $ stack exec -- ircbrowse ircbrowse.conf create-version

Run:

    $ stack exec -- ircbrowse ircbrowse.conf

Periodically update the database with new events from logs:

    $ stack exec -- ircbrowse ircbrowse.conf complete-import
