This is a GNU Emacs interface to the PubMed database of references on life
sciences and biomedical topics.

Since May 1, 2018, NCBI limits access to the E-utilities unless you have an
API key. See
<https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/>.
If you don't have an API key, E-utilities will still work, but you may be
limited to fewer requests than allowed with an API key. Any computer (IP
address) that submits more than three E-utility requests per second will
receive an error message. This limit applies to any combination of requests
to EInfo, ESearch, ESummary, EFetch, ELink, EPost, ESpell, and EGquery.

First, you will need an NCBI account. If you don't have one already, register
at <https://www.ncbi.nlm.nih.gov/account/>.

To create the key, go to the "Settings" page of your NCBI account. (Hint:
after signing in, simply click on your NCBI username in the upper right
corner of any NCBI page.) You'll see a new "API Key Management" area. Click
the "Create an API Key" button, and copy the resulting key.

Use the key by customizing the variable `pubmed-api-key' or setting the value
in your init.el or .emacs file:
(setq pubmed-api-key "1234567890abcdefghijklmnopqrstuvwxyz")

Autocompleting using PubMed suggestions

`pubmed-search' provides context aware completion via the complete-symbol
command, bound to TAB and C-M-i by default.
