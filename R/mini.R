## Mini-notation parser for maelstrom.
##
## Clean-room implementation: written from scratch against a prose-derived
## specification of the TidalCycles/Strudel mini-notation. No existing
## implementation's source was consulted.
##
## A pattern string fills one cycle; more events subdivide it.
##   "bd sd hh cp"   sequence: equal steps
##   "bd [sd cp]"    subgroup: one step, subdivided
##   "bd ~ sd"       rest (~ and - are identical)
##   "bd _ sn"       tie: _ extends the previous event one step
##   "<a b c>"       alternation: one item per cycle, rotating
##   "bd*4"          n repetitions inside the element's span
##   "bd/2"          stretched over n cycles
##   "bd!3"          n equal steps in the element's span
##   "x@3 y"         temporal weight
##   "bd, hh*8"      parallel layers over the same span
##   "hh:2"          sample index (0-based; stored, engine ignores)
##   "bd?"           random drop, 50% removal per cycle
##   "[bd|sd]"       random choice per cycle
##   "bd(3,8)"       euclidean rhythm: 3 hits over 8 steps
##   "{a b, x y z}"  polymeter: left sets the pulse
##   "0 .. 3"        range expansion: 0 1 2 3
##
## Entry points: s() for sound names, note() for pitches, n() for numbers.

## -- AST: type-tagged lists --------------------------------------------------

nd_atom    <- function(token) list(type = "atom", token = token)
nd_rest    <- function() list(type = "rest")
nd_seq     <- function(steps) list(type = "seq", steps = steps)
nd_group   <- function(inner) list(type = "group", inner = inner)
nd_alt     <- function(items) list(type = "alt", items = items)
nd_choice  <- function(items) list(type = "choice", items = items)
nd_par     <- function(layers) list(type = "par", layers = layers)
nd_rep     <- function(node, n) list(type = "rep", node = node, n = n)
nd_slow    <- function(node, n) list(type = "slow", node = node, n = n)
#' Build a degrade AST node
#'
#' AST constructor for the `?` suffix: drop the node with probability `p`.
#'
#' @param node The AST node to degrade.
#' @param p Drop probability.
#' @return A `degrade` AST node.
#' @examples
#' nd_degrade(mini_parse_root("bd"), 0.5)
#' @export
nd_degrade <- function(node, p) list(type = "degrade", node = node, p = p)
nd_sample  <- function(node, i) list(type = "sample", node = node, i = i)
nd_euclid  <- function(node, k, n, o) list(type = "euclid", node = node, k = k, n = n, o = o)
nd_poly    <- function(layers, left_steps, pulse_n) {
  list(type = "poly", layers = layers, left_steps = left_steps, pulse_n = pulse_n)
}

st_node  <- function(node, weight = 1) list(kind = "node", node = node, weight = weight)
st_rest  <- function() list(kind = "rest", node = NULL, weight = 1)
st_tie   <- function() list(kind = "tie", node = NULL, weight = 1)
#' Build a range step
#'
#' AST step constructor for the `..` range marker, used in `a .. b` expansions.
#'
#' @return A `range` step list.
#' @examples
#' st_range()
#' @export
st_range <- function() list(kind = "range", node = NULL, weight = 1)

#' Test whether a string is a number
#'
#' True when the string parses as numeric.
#'
#' @param s A character string.
#' @return A logical value.
#' @examples
#' is_num("1.5")
#' @export
is_num <- function(s) !is.na(suppressWarnings(as.numeric(s)))

## -- tokenizer ---------------------------------------------------------------

#' Tokenize mini-notation
#'
#' Split mini-notation text into a list of tokens: words, single characters, operators, and `..` range markers. Decimals stay glued to digits ("bd*1.5" lexes properly).
#'
#' @param text Mini-notation text.
#' @return A list of tokens, each a list of (kind, value).
#' @examples
#' mini_tokenize("bd*4")
#' @export
mini_tokenize <- function(text) {
  toks <- list()
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  i <- 1L
  specials <- c("[", "]", "<", ">", "{", "}", "(", ")", ",", "|", "~", "_", "-")
  ops <- c("*", "!", "@", "?", "%", ":", "/")
  is_space <- function(ch) ch %in% c(" ", "\t", "\n", "\r")
  while (i <= n) {
    ch <- chars[i]
    if (is_space(ch)) { i <- i + 1L; next }
    if (ch %in% specials) { toks <- c(toks, list(list("ch", ch))); i <- i + 1L; next }
    if (ch %in% ops) { toks <- c(toks, list(list("op", ch))); i <- i + 1L; next }
    if (ch == "." && i < n && chars[i + 1] == ".") {
      toks <- c(toks, list(list("range", ".."))); i <- i + 2L; next
    }
    j <- i
    while (j <= n) {
      cj <- chars[j]
      if (is_space(cj) || cj %in% specials || cj %in% ops) break
      if (cj == ".") {
        # ".." starts a range; a "." glued to digits stays in the word ("1.5")
        nxt <- if (j < n) chars[j + 1] else ""
        if (nxt == ".") break
        prev <- if (j > i) chars[j - 1] else ""
        if (j > i || grepl("^[0-9]$", nxt)) { j <- j + 1L; next }
        break
      }
      j <- j + 1L
    }
    word <- paste(chars[i:(j - 1)], collapse = "")
    if (!nzchar(word)) {
      toks <- c(toks, list(list("word", ch))); i <- i + 1L; next
    }
    toks <- c(toks, list(list("word", word)))
    i <- j
  }
  toks
}

## -- parser (recursive descent over an environment-held cursor) ---------------

#' Create a mini-notation parser cursor
#'
#' A cursor over a token list, held in an environment so recursive-descent functions share it.
#'
#' @param tokens A token list from `mini_tokenize`.
#' @return A parser environment with `toks` and `pos`.
#' @examples
#' ps <- new_parser(mini_tokenize("bd sd"))
#' @export
new_parser <- function(tokens) {
  ps <- new.env(parent = emptyenv())
  ps$toks <- tokens
  ps$pos <- 1L
  ps
}

#' Look at the current token
#'
#' The token under the cursor without consuming it, or NULL at end of input.
#'
#' @param ps A parser.
#' @return A token or NULL.
#' @examples
#' ps <- new_parser(mini_tokenize("bd")); ps_peek(ps)
#' @export
ps_peek <- function(ps) {
  if (ps$pos <= length(ps$toks)) ps$toks[[ps$pos]] else NULL
}

#' Value of the current token
#'
#' Shorthand for the value part of `ps_peek(ps)`.
#'
#' @param ps A parser.
#' @return The token value or NULL.
#' @examples
#' ps <- new_parser(mini_tokenize("bd")); ps_peek_val(ps)
#' @export
ps_peek_val <- function(ps) {
  t <- ps_peek(ps)
  if (is.null(t)) NULL else t[[2]]
}

#' Consume the current token
#'
#' Return the token under the cursor and advance past it.
#'
#' @param ps A parser.
#' @return The consumed token.
#' @examples
#' ps <- new_parser(mini_tokenize("bd")); ps_next(ps)
#' @export
ps_next <- function(ps) {
  t <- ps$toks[[ps$pos]]
  ps$pos <- ps$pos + 1L
  t
}

#' Consume an expected token
#'
#' Consume the current token when its value equals `val`, else stop with an error.
#'
#' @param ps A parser.
#' @param val The expected token value.
#' @return The consumed token, invisibly or as value.
#' @examples
#' ps <- new_parser(mini_tokenize("bd")); ps_expect(ps, "bd")
#' @export
ps_expect <- function(ps, val) {
  t <- ps_peek(ps)
  if (is.null(t) || t[[2]] != val) {
    stop(paste0("expected '", val, "', got ",
                if (is.null(t)) "end of input" else paste0("'", t[[2]], "'")))
  }
  ps_next(ps)
}

#' Test for an operator token
#'
#' True when the current token is the operator `op`.
#'
#' @param ps A parser.
#' @param op An operator character like "*" or "?".
#' @return A logical value.
#' @examples
#' ps <- new_parser(mini_tokenize("bd*4")); ps_next(ps); ps_is_op(ps, "*")
#' @export
ps_is_op <- function(ps, op) {
  t <- ps_peek(ps)
  !is.null(t) && t[[1]] == "op" && t[[2]] == op
}

#' Parse a numeric token
#'
#' Consume one word token that parses as a number.
#'
#' @param ps A parser.
#' @return A numeric value.
#' @examples
#' ps <- new_parser(mini_tokenize("4")); mini_parse_number(ps)
#' @export
mini_parse_number <- function(ps) {
  t <- ps_next(ps)
  if (t[[1]] != "word" || !is_num(t[[2]])) {
    stop(paste0("expected a number, got '", t[[2]], "'"))
  }
  as.numeric(t[[2]])
}

#' Count steps in a layer node
#'
#' The number of steps a parsed layer contributes (1 for non-sequences).
#'
#' @param layer A parsed AST node.
#' @return An integer step count.
#' @examples
#' mini_count_steps(mini_parse_root("bd sd"))
#' @export
mini_count_steps <- function(layer) {
  if (layer$type == "seq") max(length(layer$steps), 1) else 1
}

#' Parse comma-separated layers
#'
#' Parse parallel layers separated by commas into one AST node.
#'
#' @param ps A parser.
#' @return An AST node (a `par` node when there are several layers).
#' @examples
#' mini_parse_pattern(new_parser(mini_tokenize("bd, hh*8")))
#' @export
mini_parse_pattern <- function(ps) {
  layers <- list(mini_parse_layer(ps))
  while (identical(ps_peek_val(ps), ",")) {
    ps_next(ps)
    layers <- c(layers, list(mini_parse_layer(ps)))
  }
  if (length(layers) == 1) layers[[1]] else nd_par(layers)
}

#' Parse one mini-notation layer
#'
#' Parse a sequence of steps, expanding `!n` replication and `a .. b` ranges.
#'
#' @param ps A parser.
#' @return A `seq` AST node.
#' @examples
#' mini_parse_layer(new_parser(mini_tokenize("bd sd")))
#' @export
mini_parse_layer <- function(ps) {
  steps <- list()
  repeat {
    t <- ps_peek(ps)
    if (is.null(t) || t[[2]] %in% c(",", "]", ">", "}")) break
    st <- mini_parse_step(ps)
    ## `!n`: replicate the step n times as equal siblings
    if (ps_is_op(ps, "!")) {
      ps_next(ps)
      k <- max(as.integer(mini_parse_number(ps)), 1L)
      for (i in seq_len(k - 1)) steps <- c(steps, list(st))
    }
    steps <- c(steps, list(st))
  }
  ## range expansion: a .. b
  out <- list()
  i <- 1L
  while (i <= length(steps)) {
    st <- steps[[i]]
    if (st$kind == "node" && st$node$type == "atom" && is_num(st$node$token) &&
        i + 2 <= length(steps) &&
        steps[[i + 1]]$kind == "range" &&
        steps[[i + 2]]$kind == "node" &&
        steps[[i + 2]]$node$type == "atom" &&
        is_num(steps[[i + 2]]$node$token)) {
      a <- as.integer(as.numeric(st$node$token))
      b <- as.integer(as.numeric(steps[[i + 2]]$node$token))
      for (v in seq(min(a, b), max(a, b))) {
        out <- c(out, list(st_node(nd_atom(as.character(v)), 1)))
      }
      i <- i + 3
    } else {
      out <- c(out, list(st))
      i <- i + 1
    }
  }
  nd_seq(out)
}

#' Parse one step
#'
#' Parse a rest, tie, range marker, or a node with an optional `@weight`.
#'
#' @param ps A parser.
#' @return A step list with `kind`, `node`, and `weight`.
#' @examples
#' mini_parse_step(new_parser(mini_tokenize("bd")))
#' @export
mini_parse_step <- function(ps) {
  t <- ps_peek(ps)
  if (is.null(t)) stop("unexpected end of input")
  if (t[[2]] %in% c("~", "-")) {
    ps_next(ps)
    kind <- "rest"; node <- NULL
  } else if (t[[2]] == "_") {
    ps_next(ps)
    return(st_tie())
  } else if (t[[1]] == "range") {
    ps_next(ps)
    return(st_range())
  } else {
    node <- mini_parse_choice(ps)
    kind <- "node"
  }
  weight <- 1
  if (ps_is_op(ps, "@")) {
    ps_next(ps)
    weight <- mini_parse_number(ps)
  }
  list(kind = kind, node = node, weight = weight)
}

#' Parse a random choice
#'
#' Parse `a | b | c` into a choice node (one item per cycle).
#'
#' @param ps A parser.
#' @return An AST node.
#' @examples
#' mini_parse_choice(new_parser(mini_tokenize("bd|sd")))
#' @export
mini_parse_choice <- function(ps) {
  items <- list(mini_parse_element(ps))
  while (identical(ps_peek_val(ps), "|")) {
    ps_next(ps)
    items <- c(items, list(mini_parse_element(ps)))
  }
  if (length(items) == 1) items[[1]] else nd_choice(items)
}

#' Parse an element with suffixes
#'
#' Parse a primary, then bind `*`, `/`, `?`, `:`, and `(,)` suffixes tighter than `|`.
#'
#' @param ps A parser.
#' @return An AST node.
#' @examples
#' mini_parse_element(new_parser(mini_tokenize("bd*4")))
#' @export
mini_parse_element <- function(ps) {
  node <- mini_parse_primary(ps)
  mini_parse_suffixes(ps, node)
}

#' Parse a primary
#'
#' Parse a word atom (including the `6%5` ratio shorthand), a `[...]` group, a `<...>` alternation, or a `{...}` polymeter.
#'
#' @param ps A parser.
#' @return An AST node.
#' @examples
#' mini_parse_primary(new_parser(mini_tokenize("bd")))
#' @export
mini_parse_primary <- function(ps) {
  t <- ps_peek(ps)
  if (is.null(t)) stop("unexpected end of input")
  kind <- t[[1]]; val <- t[[2]]
  if (kind == "word") {
    ps_next(ps)
    ## ratio shorthand: 6%5 -> 1.2
    if (is_num(val) && ps_is_op(ps, "%")) {
      ps_next(ps)
      nt <- ps_next(ps)
      if (nt[[1]] != "word" || !is_num(nt[[2]])) {
        stop(paste0("expected number after %, got '", nt[[2]], "'"))
      }
      denom <- as.numeric(nt[[2]])
      if (denom == 0) {
        stop("ratio denominator after % must be nonzero")
      }
      return(nd_atom(as.character(as.numeric(val) / denom)))
    }
    return(nd_atom(val))
  }
  if (val == "[") {
    ps_next(ps)
    layer <- mini_parse_layer(ps)
    ps_expect(ps, "]")
    return(nd_group(layer))
  }
  if (val == "<") {
    ps_next(ps)
    items <- list()
    repeat {
      pt <- ps_peek(ps)
      if (is.null(pt)) stop("unclosed <")
      if (pt[[2]] == ">") { ps_next(ps); break }
      st <- mini_parse_step(ps)
      reps <- 1L
      if (ps_is_op(ps, "!")) {
        ps_next(ps)
        reps <- max(as.integer(mini_parse_number(ps)), 1L)
      }
      for (i in seq_len(reps)) {
        if (st$kind == "node") items <- c(items, list(st$node))
        else if (st$kind == "rest") items <- c(items, list(nd_rest()))
      }
    }
    return(nd_alt(items))
  }
  if (val == "{") {
    ps_next(ps)
    layers <- list(mini_parse_layer(ps))
    while (identical(ps_peek_val(ps), ",")) {
      ps_next(ps)
      layers <- c(layers, list(mini_parse_layer(ps)))
    }
    ps_expect(ps, "}")
    pulse_n <- NULL
    if (ps_is_op(ps, "%")) {
      ps_next(ps)
      pulse_n <- as.integer(mini_parse_number(ps))
    }
    if (length(layers) == 1) {
      left_steps <- if (!is.null(pulse_n)) pulse_n else mini_count_steps(layers[[1]])
      return(nd_poly(layers, left_steps, pulse_n))
    }
    return(nd_poly(layers, mini_count_steps(layers[[1]]), pulse_n))
  }
  stop(paste0("unexpected token '", val, "'"))
}

#' Parse element suffixes
#'
#' Repeatedly consume `*n` (repeat), `/n` (slow), `?` (degrade), `:i` (sample index), and `(k,n,o)` (euclidean) suffixes. `!` is left for the layer level.
#'
#' @param ps A parser.
#' @param node The AST node the suffixes apply to.
#' @return An AST node.
#' @examples
#' mini_parse_suffixes(new_parser(mini_tokenize("*4")), mini_parse_root("bd"))
#' @export
mini_parse_suffixes <- function(ps, node) {
  repeat {
    t <- ps_peek(ps)
    if (is.null(t)) return(node)
    ## NOTE: `!` is not consumed here; it replicates the whole step as equal
    ## siblings and is handled by mini_parse_layer / the <...> loop.
    if (ps_is_op(ps, "*")) {
      ps_next(ps); node <- nd_rep(node, mini_parse_number(ps))
    } else if (ps_is_op(ps, "/")) {
      ps_next(ps); node <- nd_slow(node, mini_parse_number(ps))
    } else if (ps_is_op(ps, "?")) {
      ps_next(ps)
      p <- 0.5
      pt <- ps_peek(ps)
      if (!is.null(pt) && pt[[1]] == "word" && is_num(pt[[2]])) {
        p <- as.numeric(ps_next(ps)[[2]])
      }
      node <- nd_degrade(node, p)
    } else if (ps_is_op(ps, ":")) {
      ps_next(ps); node <- nd_sample(node, as.integer(mini_parse_number(ps)))
    } else if (!is.null(t) && t[[1]] == "ch" && t[[2]] == "(") {
      node <- mini_parse_euclid(ps, node)
    } else {
      return(node)
    }
  }
}

#' Parse a euclidean suffix
#'
#' Parse `(k,n)` or `(k,n,o)` after consuming the opening paren.
#'
#' @param ps A parser positioned at the `(` token.
#' @param node The AST node the rhythm applies to.
#' @return A `euclid` AST node.
#' @examples
#' mini_parse_euclid(new_parser(mini_tokenize("(3,8)")), mini_parse_root("bd"))
#' @export
mini_parse_euclid <- function(ps, node) {
  ps_expect(ps, "(")
  k <- as.integer(mini_parse_number(ps))
  ps_expect(ps, ",")
  n <- as.integer(mini_parse_number(ps))
  o <- 0L
  if (identical(ps_peek_val(ps), ",")) {
    ps_next(ps)
    o <- as.integer(mini_parse_number(ps))
  }
  ps_expect(ps, ")")
  if (n <= 0) stop("euclidean steps must be positive")
  nd_euclid(node, k, n, o)
}

#' Parse mini-notation to an AST
#'
#' Tokenize and parse full mini-notation text; errors on trailing tokens.
#'
#' @param text Mini-notation text.
#' @return An AST node for one cycle.
#' @examples
#' mini_parse_root("bd*4")
#' @export
mini_parse_root <- function(text) {
  tokens <- mini_tokenize(text)
  if (length(tokens) == 0) return(nd_seq(list()))
  ps <- new_parser(tokens)
  node <- mini_parse_pattern(ps)
  if (!is.null(ps_peek(ps))) {
    stop(paste0("unexpected trailing token '", ps_peek_val(ps), "'"))
  }
  node
}

## -- compiler: AST -> Pattern -------------------------------------------------

#' Token to hap value
#'
#' Convert one atom token to a hap value list according to the mode.
#'
#' @param token The atom token string.
#' @param mode One of "sound", "note", "n", or "token".
#' @return A named list such as `list(sound = "bd")`.
#' @examples
#' mini_token_value("bd", "sound")
#' @export
mini_token_value <- function(token, mode) {
  if (mode == "sound") {
    parts <- strsplit(token, ":", fixed = TRUE)[[1]]
    if (length(parts) > 1) {
      list(sound = parts[1], sample = as.integer(parts[2]))
    } else {
      list(sound = token)
    }
  } else if (mode == "note") {
    list(note = token)
  } else if (mode == "n") {
    list(n = as.numeric(token))
  } else {
    list(token = token)
  }
}

#' Pattern holding one value over a span
#'
#' A single hap with the given value sounding over `span`. The span is snapshotted so loop-built closures do not capture a changing variable.
#'
#' @param value A hap value list.
#' @param span A TimeSpan.
#' @return A pattern.
#' @examples
#' query_arc(atom_pattern(list(sound = "bd"), timespan(0, 1)), 0, 1)
#' @export
atom_pattern <- function(value, span) {
  span <- force(span)  # snapshot: callers may build spans in a loop
  ms_pattern(function(q) {
    part <- ts_intersection(span, q)
    if (is.null(part)) list() else list(hap(span, part, value))
  })
}

#' Compile an AST node to a pattern
#'
#' Recursively compile a parsed AST node over `span` in the given mode.
#'
#' @param node An AST node.
#' @param span A TimeSpan.
#' @param mode One of "sound", "note", "n", or "token".
#' @return A pattern.
#' @examples
#' query_arc(mini_compile(mini_parse_root("bd"), timespan(0, 1), "sound"), 0, 1)
#' @export
mini_compile <- function(node, span, mode) {
  span <- force(span)  # snapshot for the same reason
  type <- node$type
  if (type == "atom") return(atom_pattern(mini_token_value(node$token, mode), span))
  if (type == "rest") return(silence())
  if (type == "group") return(mini_compile(node$inner, span, mode))
  if (type == "seq") return(mini_compile_seq(node$steps, span, mode))
  if (type == "par") {
    pats <- lapply(node$layers, function(layer) mini_compile(layer, span, mode))
    return(Reduce(pat_overlay, pats))
  }
  if (type == "alt") {
    items <- node$items
    if (length(items) == 0) items <- list(nd_rest())
    return(ms_pattern(function(q) {
      out <- list()
      for (cyc in seq(floor(q$begin), ceiling(q$end) - 1)) {
        sub <- ts_intersection(timespan(cyc, cyc + 1), q)
        if (is.null(sub)) next
        item <- items[[cyc %% length(items) + 1L]]
        out <- c(out, mini_compile(item, timespan(cyc, cyc + 1), mode)(sub))
      }
      out
    }))
  }
  if (type == "choice") {
    items <- node$items
    return(ms_pattern(function(q) {
      out <- list()
      for (cyc in seq(floor(q$begin), ceiling(q$end) - 1)) {
        sub <- ts_intersection(timespan(cyc, cyc + 1), q)
        if (is.null(sub)) next
        idx <- floor(uzu_runif(1, paste0("uzu:choice:", cyc)) * length(items)) + 1L
        out <- c(out, mini_compile(items[[idx]], timespan(cyc, cyc + 1), mode)(sub))
      }
      out
    }))
  }
  if (type == "rep") {
    n <- node$n
    if (n <= 0) return(silence())
    full <- as.integer(floor(n))
    frac <- n - full
    d <- ts_duration(span) / n
    pats <- list()
    cursor <- span$begin
    for (i in seq_len(full)) {
      pats <- c(pats, list(mini_compile(node$node, timespan(cursor, cursor + d), mode)))
      cursor <- cursor + d
    }
    if (frac > 1e-9) {
      pats <- c(pats, list(mini_compile(node$node, timespan(cursor, cursor + frac * d), mode)))
    }
    return(Reduce(pat_overlay, pats))
  }
  if (type == "slow") {
    n <- node$n
    if (n <= 0) stop("slow factor must be positive")
    period <- ts_duration(span) * n
    anchor <- floor(span$begin / period) * period
    return(ms_pattern(function(q) {
      out <- list()
      k0 <- floor((q$begin - anchor) / period)
      k1 <- ceiling((q$end - anchor) / period)
      for (k in seq(k0, k1 - 1)) {
        e <- timespan(anchor + k * period, anchor + (k + 1) * period)
        sub <- ts_intersection(e, q)
        if (!is.null(sub)) {
          out <- c(out, mini_compile(node$node, e, mode)(sub))
        }
      }
      out
    }))
  }
  if (type == "degrade") {
    return(degrade(mini_compile(node$node, span, mode), node$p))
  }
  if (type == "sample") {
    return(pat_with_value(mini_compile(node$node, span, mode), function(v) {
      v$sample <- node$i
      v
    }))
  }
  if (type == "euclid") {
    pulses <- bjorklund(node$k, node$n)
    o <- node$o %% node$n
    if (o > 0) pulses <- c(pulses[(node$n - o + 1):node$n], pulses[1:(node$n - o)])
    steps <- lapply(pulses, function(p) {
      if (p) st_node(node$node, 1) else st_rest()
    })
    return(mini_compile_seq(steps, span, mode))
  }
  if (type == "poly") {
    return(mini_compile_poly(node, span, mode))
  }
  stop(paste0("unknown node type: ", type))
}

#' Compile a step sequence
#'
#' Lay out steps with temporal weights over `span`, folding ties into the previous segment.
#'
#' @param steps A list of step lists.
#' @param span A TimeSpan.
#' @param mode Compile mode.
#' @return A pattern.
#' @examples
#' steps <- list(list(kind = "node", node = mini_parse_root("bd"), weight = 1),
#'                list(kind = "node", node = mini_parse_root("sd"), weight = 1))
#' query_arc(mini_compile_seq(steps, timespan(0, 1), "sound"), 0, 1)
#' @export
mini_compile_seq <- function(steps, span, mode) {
  total <- sum(vapply(steps, function(st) st$weight, numeric(1)))
  if (total <= 0 || length(steps) == 0) return(silence())
  ## fold ties into the previous segment
  segs <- list() ## each: list(node, wbegin, wend)
  cursor <- 0
  for (st in steps) {
    w <- st$weight
    if (st$kind == "tie") {
      if (length(segs) > 0) {
        prev <- segs[[length(segs)]]
        segs[[length(segs)]] <- list(node = prev$node, wbegin = prev$wbegin, wend = prev$wend + w)
      }
      cursor <- cursor + w
    } else if (st$kind %in% c("rest", "range")) {
      cursor <- cursor + w
    } else {
      segs <- c(segs, list(list(node = st$node, wbegin = cursor, wend = cursor + w)))
      cursor <- cursor + w
    }
  }
  pats <- lapply(segs, function(sg) {
    part <- timespan(span$begin + ts_duration(span) * sg$wbegin / total,
                     span$begin + ts_duration(span) * sg$wend / total)
    mini_compile(sg$node, part, mode)
  })
  if (length(pats) == 0) return(silence())
  Reduce(pat_overlay, pats)
}

#' Compile a polymeter node
#'
#' The left layer sets the pulse; other layers phase-lock to it across cycle boundaries.
#'
#' @param node A `poly` AST node.
#' @param span A TimeSpan.
#' @param mode Compile mode.
#' @return A pattern.
#' @examples
#' node <- mini_parse_root("{bd, hh*2}")$steps[[1]]$node
#' query_arc(mini_compile_poly(node, timespan(0, 1), "sound"), 0, 1)
#' @export
mini_compile_poly <- function(node, span, mode) {
  layers <- node$layers
  left <- mini_compile(layers[[1]], span, mode)
  if (length(layers) < 2) return(left)
  right_layer <- layers[[2]]
  rsteps <- if (right_layer$type == "seq") right_layer$steps else list(st_node(right_layer, 1))
  rnodes <- lapply(Filter(function(st) st$kind == "node", rsteps), function(st) st$node)
  if (length(rnodes) == 0) return(left)
  pulse <- ts_duration(span) / node$left_steps
  right_period <- length(rnodes) * pulse
  anchor <- floor(span$begin / right_period) * right_period
  ms_pattern(function(q) {
    out <- left(q)
    i0 <- floor((q$begin - anchor) / pulse)
    i1 <- ceiling((q$end - anchor) / pulse)
    for (i in seq(i0, i1 - 1)) {
      slot <- timespan(anchor + i * pulse, anchor + (i + 1) * pulse)
      sub <- ts_intersection(slot, q)
      if (is.null(sub)) next
      idx <- i %% length(rnodes) + 1L
      out <- c(out, mini_compile(rnodes[[idx]], slot, mode)(sub))
    }
    out
  })
}

## Tile the compiled one-cycle pattern across all cycles, recompiling per
## cycle so cycle-relative nodes (alt, choice, slow, poly) see absolute
## cycle numbers.
#' Tile a one-cycle AST across cycles
#'
#' Recompile the root AST per cycle so cycle-relative nodes (alternation, choice, slow, polymeter) see absolute cycle numbers.
#'
#' @param root An AST node for one cycle.
#' @param mode Compile mode.
#' @return A pattern.
#' @examples
#' query_arc(cycle_driver(mini_parse_root("bd"), "sound"), 0, 2)
#' @export
cycle_driver <- function(root, mode) {
  ms_pattern(function(q) {
    out <- list()
    for (cyc in seq(floor(q$begin), ceiling(q$end) - 1)) {
      sub <- ts_intersection(timespan(cyc, cyc + 1), q)
      if (is.null(sub)) next
      out <- c(out, mini_compile(root, timespan(cyc, cyc + 1), mode)(sub))
    }
    out
  })
}

## -- entry points ---------------------------------------------------------------

#' Parse mini-notation to a token pattern
#'
#' Entry point for raw token patterns (values are `list(token = ...)`).
#'
#' @param text Mini-notation text.
#' @return A pattern.
#' @examples
#' query_arc(parse_mini("a b"), 0, 1)
#' @export
parse_mini <- function(text) cycle_driver(mini_parse_root(text), "token")

#' Mini-notation for drum sounds
#'
#' Parse text into a pattern of `list(sound = ...)` values, one cycle per input cycle.
#'
#' @param text Mini-notation text.
#' @return A pattern.
#' @examples
#' query_arc(s("bd sd"), 0, 1)
#' @export
s <- function(text) cycle_driver(mini_parse_root(text), "sound")

#' Mini-notation for pitches
#'
#' Parse text into a pattern of `list(note = ...)` values.
#'
#' @param text Mini-notation text.
#' @return A pattern.
#' @examples
#' query_arc(note("c4 eb4 g4"), 0, 1)
#' @export
note <- function(text) cycle_driver(mini_parse_root(text), "note")

#' Mini-notation for numbers
#'
#' Parse text into a pattern of `list(n = ...)` values. Ratios like `6%5` are preserved as numbers (1.2).
#'
#' @param text Mini-notation text.
#' @return A pattern.
#' @examples
#' query_arc(n("0 1 2 3"), 0, 1)
#' @export
n <- function(text) cycle_driver(mini_parse_root(text), "n")
