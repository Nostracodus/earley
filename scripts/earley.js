/* MIT License
 * Copyright (c) 2022 Cody Miller.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this
 * software and associated documentation files (the "Software"), to deal in the Software
 * without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies
 * or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * --------------------------------------------------------------------------------------
 *
 * This file implements the Earley parsing algorithm outlined by Elizabeth Scott in
 * Recognition is not Parsing - SPPF-style parsing from cubic recognizers.
 *
 * Typically, Earley recognizers are turned into Earley parsers by adding links between
 * Earley items. Once parsing is finished, these links can be walked to reveal
 * derivations. Scott's algorithm instead builds a Shared Packed Parse Forest during
 * parsing. This data structure represents all possible derivations (including
 * potentially infinite derivations) of an input string according to a grammar. This
 * algorithm (when properly implemented) is O(n^3).
 *
 * This implementation aims to support visualization of these constructed SPPFs so that a
 * user may gain an intuition for the way the algorithm operates. It is liberally
 * commented so that a user may read the source to view an executable implementation of
 * the pseudocode in the original paper. Some amount of familiarity with an Earley
 * recognizer is expected, but terminology is typically defined.
 * 
 * The implementation is not designed for performance. In particular, searching for
 * families of children within a node is not constant. This means that the implementation
 * is not truly O(n^3) as promised. The children of a node are perhaps better implemented
 * as a linked list so as to avoid allocating an additional array per node.
 */

/**
 * @returns the first element of an array.
 */
Array.prototype.front = function () {
  return this[0];
};

/**
 * @returns the last element of an array.
 */
Array.prototype.back = function () {
  return this[this.length - 1];
};

/**
 * @returns true if the array contains no elements.
 */
Array.prototype.empty = function () {
  return this.length === 0;
};

/**
 * Constructs a Slot, a representation of a dotted rule.
 * @constructor
 * @param subject {string} The nonterminal that this rule names.
 * @param terms {string[]} The terms that this rule comprises.
 * @param dot {number} Indication of the progress made in recognizing the rule.
 *
 * An Earley parser fundamentally works with dotted rules. These are typically written as
 * S -> A · B c. The nonterminal to the left of the arrow is called the 'subject' in this
 * implementation. The terms to the right of the arrow are the 'terms' of the rule. The
 * dot indicates how much progress has been made in recognizing this rule; terms to the
 * left of the dot have been matched, terms to the right remain to be matched.
 *
 * If there are no terms to the right of the dot (i.e. it is at the end of the rule), the
 * rule is completely matched and is called 'complete' in this implementation.
 *
 * The term to the right of the dot (if any) is called the 'awaited term' here.
 */
function Slot(subject, terms, dot) {
  this.subject = subject;
  this.terms = terms;
  this.dot = dot;
}

/**
 * Constructs a Symbol node, representing the derivation of a nonterminal.
 * @constructor
 * @param subject {string} The nonterminal represented by this node.
 * @param start {number} The inclusive index where this node began being matched.
 * @param end {number} The exclusive index where this node was completely matched.
 * @param children {Node[]} The sub-derivations of this node.
 */
function Symbol(subject, start, end, children) {
  this.subject = subject;
  this.start = start;
  this.end = end;
  this.children = children;
}

/**
 * Constructs a Terminal node, representing the derivation of a terminal.
 * @constructor
 * @param terminal {string} The terminal represented by this node.
 * @param start {number} The inclusive index where this node began being matched.
 * @param end {number} The exclusive index where this node was completely matched.
 */
function Terminal(terminal, start, end) {
  this.terminal = terminal;
  this.start = start;
  this.end = end;
}

/**
 * Construct an Epsilon node, indicating that the empty string was matched.
 * @constructor
 * @param start {number} The inclusive index where this node was matched.
 */
function Epsilon(start) {
  this.start = start;
  this.end = start;
}

/**
 * Constructs an Intermediate node, representing an incomplete grammar slot.
 * @constructor
 * @param slot {Slot} The active grammar slot this node represents.
 * @param start {number} The inclusive index where this node began being matched.
 * @param end {number} The exclusive index where this node finished being matched.
 * @param children {Node[]} The sub-derivations of this node.
 */
function Intermediate(slot, start, end, children) {
  this.slot = slot;
  this.start = start;
  this.end = end;
  this.children = children;
}

/**
 * Constructs a Pack node, representing one derivation in an ambiguous derivation.
 * @param start {number} The inclusive index where this node began being matched.
 * @param end {number} The exclusive index where this node finished being matched.
 * @param children {Node[]} The sub-derivations of this node.
 */
function Pack(start, end, children) {
  this.start = start;
  this.end = end;
  this.children = children;
}

/**
 * An alias for any SPPF node.
 * @typedef {(Symbol|Terminal|Epsilon|Intermediate|Pack)} Node
 */

/**
 * Constructs an Item.
 * @constructor
 * @param slot {Slot} The dotted rule that is being parsed.
 * @param origin {number} The index that this Item began being parsed.
 * @param node {Node} The derivation of this Item.
 *
 * Items are a combination of a dotted rule (a Slot) and some bookkeeping details that
 * that are core to the Earley parsing algorithm. Each Item has an 'origin', which is a
 * number that records the index in the input string in which a dotted rule began being
 * matched. This is used to perform the 'completion' step of the algorithm.
 *
 * The parsing algorithm additionally adds an SPPF node to the Item. This is used to
 * attach in-progress derivations with items as they are parsed. Details of the attached
 * node are outlined throughout the implementation.
 */
function Item(slot, origin, node) {
  this.slot = slot;
  this.origin = origin;
  this.node = node;
}

/**
 * Constructs a Grammar from the given array of rule strings.
 * @constructor
 * @param rules {string[]} A string of raw rule strings.
 *
 * Given an array of rules of the form "S -> A B c", constructs a grammar that is
 * suitable for parsing.
 */
function Grammar(rules) {
  /* The internal representation here is not as efficient as it could be for a
   * production parser. In a production parser, the rules should be assigned monotonic
   * ids. This would allow rules to be stored in a simple array with efficient constant
   * access. This representation just uses a mapping from a string to an array of term
   * arrays.
   */
  this.rules = {};

  /* The initial rule is customarily added to the grammar. Assuming the user's start
   * rule is called 'S', this initial rule is ^ -> S. This rule is added to ensure that
   * the start rule is never used in the body of any rules in the grammar. The start
   * rule's name is arbitrary - all that matters is that we apply it consistently and
   * that the user does not have a rule of the same name.
   */
  this.initialSubject = "^";

  rules.forEach((rule) => {
    const [subject, _, ...terms] = rule.trim().split(" ");

    if (Object.keys(this.rules).length === 0)
      this.rules[this.initialSubject] = [[subject]];

    this.rules[subject] = this.rules[subject] || [];
    this.rules[subject].push(terms);
  });
}

/**
 * Constructs a Parser for the given input over the given grammar.
 * @constructor
 */
function Parser(grammar, input) {
  this.grammar = grammar;

  /* Input represents the tokenized input stream. */
  this.input = input;

  /* Set to an index if there is a syntax error in the provded input. */
  this.error = undefined;

  /* The "Earley sets" of items that are constructed during parsing. Each token in the
   * user's input has a corresponding set of items.
   *
   * The name 'chart' here is derived from
   * [chart parsing](https://en.wikipedia.org/wiki/Chart_parser), of which an Earley
   * parser is a particular instance.
   */
  this.chart = [];

  /* Symbol nodes for nullable nonterminals in the current Earley set. This is called
   * the 'H' set in Scott's paper. This is used to handle a special case that can result
   * in a bug in Earley parsers. See the implementation for details.
   */
  this.completedNullables = {};

  /* The items that are awaiting a terminal in the current Earley set. The only action
   * these items can lead to are scans. This is the Q set in Scott's paper.
   */
  this.awaitingScan = [];

  /* This tracks items that need to be processed via completion or predictions. This is
   * the R set in Scott's paper.
   */
  this.awaitingPredictionCompletion = [];

  /* The node of a complete item with the augmented start rule as its subject. */
  this.accepted = null;

  /* A set of nonterminals that have been predicted at the current input index. */
  this.predicted = [];

  /* The current index into the token stream. */
  this.index = 0;

  /* The set of intermediate nodes that were created at the current index. */
  this.createdIntermediateNodes = [];

  /* The set of symbol nodes that were created at the current index. */
  this.createdSymbolNodes = {};

  /* The single epsilon node that may be created at the current index. */
  this.epsilonNode = undefined;
}

/**
 * Determines if two slots refer to the same grammar position.
 * @param s1 {Slot} The first slot.
 * @param s2 {Slot} The second slot.
 * @returns {boolean} True if the slots are equivalent, false otherwise.
 */
function slotEquivalent(s1, s2) {
  if (s1.subject !== s2.subject) return false;
  if (s1.terms !== s2.terms) return false;
  return s1.dot === s2.dot;
}

/**
 * Determines if two items contain the same information.
 * @param it1 {Item} The first item.
 * @param it2 {Item} The second item.
 * @returns {boolean} True if the items are equivalent, false otherwise.
 */
function itemEquivalent(it1, it2) {
  if (!slotEquivalent(it1.slot, it2.slot)) return false;
  if (it1.origin !== it2.origin) return false;
  return it1.node === it2.node;
}

/**
 * Determines if the array contains the given item.
 * @param array {Item[]} The array of items
 * @param item {Item} The item to check for.
 * @returns {boolean} True if the array contains the given item, false otherwise.
 */
function containsItem(array, item) {
  for (let i = 0; i < array.length; ++i) {
    if (itemEquivalent(array[i], item)) return true;
  }

  return false;
}

/**
 * Constructs an Item representing the next grammar slot.
 * @param item {Item} The item to advance.
 * @returns {Item} The advanced item.
 */
function advanceItem(item) {
  const slot = new Slot(item.slot.subject, item.slot.terms, item.slot.dot + 1);
  return new Item(slot, item.origin, item.node);
}

/**
 * Returns whether this item is completely parsed.
 * @param item {Item} The item to check.
 * @returns {boolean} True if this item's slot is complete.
 */
function isComplete(item) {
  return item.slot.dot >= item.slot.terms.length;
}

/**
 * Returns the nonterminal that the Item is waiting to be parsed before continuing.
 * @param item {Item} The item to operate on.
 * @returns {string} The nonterminal the item is waiting on.
 */
function awaiting(item) {
  return item.slot.terms[item.slot.dot];
}

/**
 * Determines if the term after the dot in the given item's slot is a nonterminal
 * @param grammar {Grammar} The grammar to look up a nonterminal in.
 * @param item {Item} The item to inspect.
 * @returns {boolean} True if the item is awaiting a nonterminal, false otherwise.
 */
function isAwaitingNonterminal(grammar, item) {
  return grammar.rules[awaiting(item)] !== undefined;
}

/**
 * Places the provided item in the appropriate queue in the provided parser.
 * @param parser {Parser} The parser state to augment.
 * @param item {Item} The item to enqueue.
 */
function enqueueItem(parser, item) {
  /* Items are added to the chart as a means of detecting duplicate items. */
  parser.chart.back().push(item);
  if (isComplete(item) || isAwaitingNonterminal(parser.grammar, item))
    parser.awaitingPredictionCompletion.push(item);
  else parser.awaitingScan.push(item);
}

/**
 * Performs the prediction step of the Earley algorithm.
 * @param parser {Parser}
 * @param nonterminal {string} The nonterminal to predict.
 *
 * The Prediction step is the same as in an Earley recognizer. The rule has not matched
 * any input, so there is no node attached to predicted items. Repeated predictions yield
 * identical items and so can be avoided if the same nonterminal is predicted multiple
 * times in the same set.
 */
function predict(parser, nonterminal) {
  /* Grammars like the following can result in repeated predictions of the same
   * nonterminal:
   *
   *    S -> A
   *    A -> B b
   *    A -> B c
   */
  if (parser.predicted.includes(nonterminal)) return;

  parser.predicted.push(nonterminal);
  parser.grammar.rules[nonterminal].forEach((terms) => {
    const slot = new Slot(nonterminal, terms, 0);
    const item = new Item(slot, parser.index, null);
    enqueueItem(parser, item);
  });
}

/**
 * Performs the 'init' step of the Earley algorithm.
 * @param parser {Parser} The parser to initialize.
 *
 * Many descriptions of Earley's algorithm don't explicitly list 'init' as a step
 * in the algorithm. It is invoked a single time to initialize the parser state.
 * It corresponds to a prediction of the augmented start nonterminal.
 */
function init(parser) {
  parser.chart.push([]);
  parser.epsilonNode = new Epsilon(0);
  predict(parser, parser.grammar.initialSubject);
}

/**
 * Performs the 'complete' step of the Earley algorithm.
 * @param parser {Parser} The parser state to augment.
 * @param item {Item} The item that has been completely recognized.
 */
function complete(parser, item) {
  if (item.origin === parser.index) {
    /* When an item is predicted and completed within the same set (or, the origin is
     * the same as the current input index), the item is nullable and will have no node
     * attached. This special case adds an Epsilon derivation to the completed item and
     * adds the nonterminal to a special set. This is used to handle repeated
     * predictions of the same nullable nonterminal in the same set. See
     * [note.nullable-nonterminals].
     */
    if (item.node == undefined)
      item.node = createNextNode(parser, item, parser.epsilonNode);
    if (parser.completedNullables[item.slot.subject] == undefined)
      parser.completedNullables[item.slot.subject] = item.node;
  }

  const originSet = parser.chart[item.origin];
  originSet.forEach((parentItem) => {
    if (!isComplete(parentItem) && awaiting(parentItem) === item.slot.subject) {
      const nitem = advanceItem(parentItem);
      nitem.node = createNextNode(parser, nitem, item.node);
      if (!containsItem(parser.chart.back(), nitem)) enqueueItem(parser, nitem);
    }
  });
}

/**
 * Performs completion or prediction for items in the awaitingPredictionCompletion queue.
 * @param parser {Parser} The parser state to augment.
 *
 * This implementation performs all predictions and completions before performing a scan
 * at a given input index. When items are enqueued, they are stored in separate sets --
 * this function 'pumps' the non-scanning set.
 */
function predictOrComplete(parser) {
  const item = parser.awaitingPredictionCompletion.shift();
  if (isComplete(item)) {
    /* If the complete nonterminal is the augmented start nonterminal, then the parser
     * has accepted the input. It is convenient to store the derivation while
     * processing the item so that we can easily give the user the derivation. */
    if (item.slot.subject === parser.grammar.initialSubject) {
      parser.accepted = item.node.children[0];
    } else {
      complete(parser, item);
    }
  } else {
    const awaited = awaiting(item);
    predict(parser, awaited);

    /* [note.nullable-nonterminals]
     * The 'completedNullables' set is required to handle the special case of a grammar
     * like the following:
     *
     *    S -> A A b
     *    A ->
     *
     * In the first Earley set, A will be predicted and completed, yielding item S -> A
     * · A b. This item will fail to predict the second 'A' because A -> · already
     * exists in the current set (this behavior is required to prevent infinite looping
     * due to left recursion in the general case). This is the same problem that Aycock
     * and Horspool discuss in their Practical Earley Parsing paper.
     */
    const nulled = parser.completedNullables[awaited];
    if (nulled !== undefined) {
      const nitem = advanceItem(item);
      nitem.node = createNextNode(parser, nitem, nulled);
      if (!containsItem(parser.chart.back(), nitem)) enqueueItem(parser, nitem);
    }
  }
}

/**
 * Determines if there are no more actions that the parser can take.
 * @param parser {Parser} The parser state to examine.
 * @returns True if the parser has no more actions, false otherwise.
 */
function isExhausted(parser) {
  return (
    !parser.chart.empty() &&
    parser.input.length === parser.index &&
    parser.awaitingPredictionCompletion.empty()
  );
}

function resetBookkeeping(parser) {
  parser.accepted = null;
  parser.awaitingScan = [];
  parser.predicted = [];
  parser.createdSymbolNodes = {};
  parser.createdIntermediateNodes = [];
  parser.completedNullables = {};
  parser.epsilonNode = undefined;
  parser.error = undefined;
}

function getSymbolNode(parser, symbol, start, end) {
  const nodes = parser.createdSymbolNodes[symbol];
  if (nodes !== undefined) {
    const existing = nodes.find(
      (node) => node.start === start && node.end === end
    );
    if (existing !== undefined) return existing;
  }
  const node = new Symbol(symbol, start, end, []);
  parser.createdSymbolNodes[symbol] ||= [];
  parser.createdSymbolNodes[symbol].push(node);
  return node;
}

function getIntermediateNode(parser, slot, start, end) {
  const existing = parser.createdIntermediateNodes.find(
    (node) =>
      slotEquivalent(node.slot, slot) && node.start === start && node.end === end
  );
  if (existing !== undefined) return existing;
  const node = new Intermediate(slot, start, end, []);
  parser.createdIntermediateNodes.push(node);
  return node;
}

/**
 * Determines if the node has sub-derivations that are the given family.
 * @param node {Node} The parent node to inspect.
 * @param family {Node[]} The children to find.
 * @returns {boolean} True if the node has the given children, false otherwise.
 *
 * Families are either [left] or [left, right].
 */
function hasFamily(node, family) {
  if (node.children === family) return true;

  if (!(node.children[0] instanceof Pack)) {
    return node.children[0] === family[0] && node.children[1] === family[1];
  }

  return node.children.find(
    (pack) => pack.children[0] === family[0] && pack.children[1] === family[1]
  ) !== undefined;
}

/**
 * Packs the given node's children into a Pack node if they aren't already packed.
 * @param node {Node} The parent node whose children will be packed.
 * @returns {void}
 */
function packChildren(node) {
  if (node.children[0] instanceof Pack) {
    return;
  }

  const cs = node.children;
  const pack = new Pack(cs.front().start, cs.back().end, node.children);
  node.children = [pack];
}

function createNextNode(parser, item, rightNode) {
  /* Items in the second slot position (e.g. S ::= A @ b) don't use an Intermediate node
   * because it represents redundant information. They instead use the node of their
   * first term.
   */
  if (!isComplete(item) && item.slot.dot <= 1) {
    return rightNode;
  }

  const leftNode = item.node;

  /* If the item is complete, the resulting node is a symbol node. Otherwise, an
   * Intermediate node is created.
   */
  const resultingNode = isComplete(item)
    ? getSymbolNode(parser, item.slot.subject, item.origin, rightNode.end)
    : getIntermediateNode(parser, item.slot, item.origin, rightNode.end);

  const family = leftNode ? [leftNode, rightNode] : [rightNode];

  /* Because we reuse nodes that have been created at the current input index, the node
   * may already have children. In the case that it doesn't, we can just append the
   * current family to the node. Otherwise, we need to check to see if the current
   * family already exists in the current node.
   *
   * This matters for grammars like the following:
   *     S -> A
   *     A -> a
   *     A -> B
   *     B -> a
   */
  if (resultingNode.children.empty()) {
    resultingNode.children = family;
  } else if (!hasFamily(resultingNode, family)) {
    packChildren(resultingNode);
    const pack = new Pack(resultingNode.start, resultingNode.end, family);
    resultingNode.children.push(pack);
  }

  return resultingNode;
}

function scan(parser, items) {
  const terminal = parser.input[parser.index];
  const terminalNode = new Terminal(terminal, parser.index, parser.index + 1);

  /* Used to determine if any of the items in the current set were successfully
   * scanned. This determines whether there was a syntax error or not. */
  let scannedSomething = false;

  parser.chart.push([]);

  items.forEach((item) => {
    if (awaiting(item) === terminal) {
      scannedSomething = true;
      const nitem = advanceItem(item);
      nitem.node = createNextNode(parser, nitem, terminalNode);
      enqueueItem(parser, nitem);
    }
  });

  if (!scannedSomething) {
    parser.error = parser.index;
  }

  ++parser.index;
}

function step(parser) {
  if (parser.chart.empty()) {
    init(parser);
  } else if (isExhausted(parser)) {
    return;
  }

  if (!parser.awaitingPredictionCompletion.empty()) {
    predictOrComplete(parser);
  } else {
    const items = parser.awaitingScan;
    resetBookkeeping(parser);
    scan(parser, items);
    parser.epsilonNode = new Epsilon(parser.index);
  }
}

const visualization = {
  objectCount: 0,

  renderSlot: function (slot) {
    const terms = [...slot.terms];
    terms.splice(slot.dot, 0, "·");
    return `${slot.subject} -> ${terms.join(" ")}`;
  },

  printItem: function (item) {
    const terms = [...item.slot.terms];
    terms.splice(item.slot.dot, 0, "·");
    const node = item.node === null ? "no-node" : "node";
    console.log(
      `${item.slot.subject} -> ${terms.join(" ")}, ${item.origin}, ${node}`
    );
  },

  printEarleySet: function (items, index) {
    console.log(`=== E${index} ===`);
    items.forEach((item) => this.printItem(item));
  },

  graphOf: function(node) {
    let count = 0;
    let ids = new WeakMap();
    let worklist = [node];

    while (!worklist.empty()) {
      const node = worklist.shift();
      let id = ids.get(node);
      if (id !== undefined)
        continue;

      ids.set(node, count++);
      let children = node.children;
      if (children !== undefined)
        worklist.push(...children);
    }

    const nodes = [];
    const edges = [];

    worklist = [node];

    const visited = [];
    while (!worklist.empty()) {
      const node = worklist.shift();
      const id = ids.get(node);
      const label = this.labelOf(node);
      const shape = this.shapeOf(node);

      if (visited.includes(node))
        continue;

      visited.push(node);

      nodes.push({id: id, label: label, shape: shape});

      let children = node.children;
      if (children !== undefined) {
        children.forEach((child, index) => {
          let cid = ids.get(child);
          edges.push({from: id, to: cid, id: edges.length, label: `${index}`});
          worklist.push(child);
        });
      }
    }

    return [nodes, edges];
  },

  labelOf: function(node) {
    if (node instanceof Symbol) {
      return `${node.subject}, ${node.start}, ${node.end}`;
    } else if (node instanceof Intermediate) {
      return `${this.renderSlot(node.slot)}, ${node.start}, ${node.end}`;
    } else if (node instanceof Terminal) {
      return `${node.terminal}, ${node.start}, ${node.end}`;
    } else if (node instanceof Epsilon) {
      return `ε, ${node.start}, ${node.end}`
    } else {
      return '';
    }
  },

  shapeOf: function(node) {
    if (node instanceof Pack) {
      return "dot";
    } else {
      return "ellipse";
    }
  },

  renderNode: function(node, theme) {
    let logBackup = console.log;
    let logMessages = [];

    console.log = function() {
        logMessages.push.apply(logMessages, arguments);
    };

    this.printNode(node, theme);
    console.log = logBackup;
    return logMessages.join('\n');
  },

  printNodes: function (node, visited) {
    if (visited.get(node) !== undefined) return visited.get(node);

    const id = this.objectCount;
    visited.set(node, id);
    ++this.objectCount;

    const visitChildren = (node) => {
      node.children.forEach((child, index) => {
        let cid = visited.get(child);
        if (cid === undefined) {
          cid = this.printNodes(child, visited);
        }
        console.log(`  node${id} -> node${cid}[label="${index}", fontsize=8];`);
      });
    };

    if (node instanceof Symbol) {
      console.log(
        `  node${id}[label="${node.subject}, ${node.start}, ${node.end}"];`
      );
      visitChildren(node);
    } else if (node instanceof Intermediate) {
      console.log(
        `  node${id}[label="${this.renderSlot(node.slot)}, ${node.start}, ${
          node.end
        }"];`
      );
      visitChildren(node, id);
    } else if (node instanceof Terminal) {
      console.log(
        `  node${id}[label="${node.terminal}, ${node.start}, ${node.end}"];`
      );
    } else if (node instanceof Epsilon) {
      console.log(
        `  node${id}[label="&epsilon;, ${node.start}, ${node.end}"];`
      );
    } else {
      console.log(`  node${id}[label="", shape=circle, width=0.1];`);
      visitChildren(node, id);
    }

    return id;
  },

  printNode: function (node, theme) {
    console.log("digraph G {");
    if (theme !== undefined) {
      console.log(`  bgcolor="${theme.bgcolor}";`);
      console.log(`  color="${theme.fgcolor}";`);
      console.log(`  node[color="${theme.fgcolor}", fontcolor="${theme.fgcolor}"];`);
      console.log(`  edge[color="${theme.fgcolor}", fontcolor="${theme.fgcolor}"];`);
    }

    console.log("  node[shape=box];");

    this.printNodes(node, new WeakMap());
    console.log("}");
  },

  printChart: function (parser) {
    for (let i = 0; i < parser.chart.length; ++i)
      this.printEarleySet(parser.chart[i], i);
  },
};

if (typeof window === "undefined") {
  exports.Grammar = Grammar;
  exports.Parser = Parser;
  exports.step = step;
  exports.isExhausted = isExhausted;
  exports.visualization = visualization;
}
