## HASKELL-ERP
This is an ERP application for learning Haskell based on Yesod web application framework by Michael Snoyman.

The SW cannot be run as such as there is a great deal of configuration information missing relating to the services accessed. Also, for creating reports the SW relies on a nodejs based piece of code for creating pdf-reports. 

I have used this software for teaching Haskell and type safe programming in general since 2022. Note that this should not be used for production. There is, for example, something to the security of this which which is still very much work in progress. Can you figure out what it is, fork your own version and propose a solution? How about writing some tests for it? 

What is demonstrated and what you could learn while dealing with this piece of software? 

* Creating and maintaining SQL data using Persistent ORM
* Using transactions for data integrity 
* Designing maintainable SQL databases and exploiting ORM if it is feasable
* Accessing file system  
* Passing settings information around by using the Reader monad
* Using middleware to do some preprocessing for the incoming HTTP requests
* Accessing Finnish Tax authority by filing VAT report to the authority
* Accessing Maventa invoicing API
* Accessing tulorekisteri API and filing wage payment related reports to the authorities
* Accessing Finnish income register to get and renew certifcates to get credentials for accessing several authorities
* Accessing Alandsbanken Open Banking API 
* Using Bang patterns to force code execution
* Using secure sockets to deal with secure HTTP endpoints
* Using GADT's to demonstrate type safe for invoicing
* Using HAXML for producing Haskell code for parsing XML
* Using Tree structures for handling account charts
* Using JWT to authenticate the users
* Using HTTP libraries to communicate with remote endpoints
* Relying on template haskell for code generation
* Using digital signatures for real use cases
* Parsing XML for real life use cases dealing with real life business entities (banks, taxation authorities, income registy, E-invoicing...)
* Learning about the ergonomics of Haskell and the lack of it, too
* Using haskell toolchain and VSCode for Haskell development
* Using typeclasses at basic level to define interfaces
* Parsing Alandsbanken pdf formatted bank statements  ... and many other things! 

In general you will learn to rely on the type checker and learn to have fun with Haskell!


## Directory structure
| Dir  | Information  |
|---|---|
| app   | Build and deploy files  |
| config  | Database table and route configurations  |
| src  |  Request Handlers and database migration files |
| static  |  *ignore/ Template static files |
| test | test cases  |

## Adding  new API endpoint
```bash
yesod add-handler
```

## Troubleshooting

1. `package.yaml vs cabal file` issue: Delete `.cabal ` from the root directory and run `stack build` and `stack run` before starting [dev server](##Development)
2. `Database migration manual intervention required. The unsafe actions are prefixed by '***' below` issue: Delete .sqlite3 file from root direcotry and start [dev server](##Development)

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag web-boilerplate-haskell:library-only --flag web-boilerplate-haskell:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
