# Fixpoint Interceptors

Interceptors for asynchronous web handlers.

Check out [the blog
post](https://lispcast.com/a-model-of-interceptors/).

## Current version

Please use the git sha method of dependencies.

## Usage

Require the library.

```clojure
(require '[fixpoint.interceptors :as int])
```

Interceptors are vectors of maps. Each map can contain three
keys, `:enter`, `:leave`, and `:error`. The values for those
keys are functions of one argument. You can compose two or
more Interceptors with the `chain` function, which will
return a new Interceptor.

When you've got your Interceptor just how you like it, you
can call `run` on it to run a value through it.

`normalize` will convert a few different types into
Interceptors. `chain` will normalize its arguments.

### Example

```clojure
(require '[fixpoint.interceptors :as int])

(def i [{:enter inc :leave inc}])

(run i 0) ;=> 2
```

## Testing

```bash
$CMD clj -Atest
```

## License

Copyright © 2018 Eric Normand

Distributed under the Eclipse Public License either version
1.0 or (at your option) any later version.
