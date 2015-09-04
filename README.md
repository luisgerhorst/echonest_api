# The Echo Nest API

This is an Erlang wrapper that allows easy access to the API of [The Echo Nest](http://the.echonest.com). It dynamically limits the number of requests according to the API's rate limit.

Because requests are performed one after another by a single worker it may not be suited for applications that require high throughput.

## Config

Simply set your [API key](http://developer.echonest.com/account/register) as environment variable before starting the application.

```erl
application:set_env(echonest_api, echonest_api_key, "my-api-key-here")
```

## Start

Start the application before making any request.

```erl
application:ensure_all_started(echonest_api)
```

## Basic Usage

Perform simple GET/POST requests with

```erl
echonest_api:get(Path :: string(), Parameters)
echonest_api:post(Path :: string(), Parameters)
Parameters :: [{Parameter :: string(), Value :: string()}]
```

where `Path` is the part of the url that comes after `http://developer.echonest.com/api/v4/`, e.g. `artist/songs`.

## Advanced Usage

For convenience some higher level functions are also exported by `echonest_api`.

Reads the contents of a taste profile in chunks of 300 items and returns them all:

```erl
read_tasteprofile(TasteProfileID :: string(), Parameters)
```

Runs until the ticket (as returned by `tasteprofile/update`) finishes:

```erl
wait_for_ticket_to_finishes(Ticket :: string())
```
