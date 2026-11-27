# buildr Visual Grammar v1 Specification

**Specification Version**: 1.0  
**Status**: Canonical Standard & Published Specification (§11, §23)  
**Reference Implementation**: [buildr](https://github.com/Bombay-Boyz/buildr)  

---

## 1. Design Philosophy: Distinction Without Colour

The foundational requirement of **buildr Visual Grammar v1** is that every compilation state, lifecycle event, and progress transition MUST be uniquely identifiable by **glyph, label, and layout alone**.

Colour is used strictly as a secondary reinforcement (WCAG 2.1 compliance). In environments without color support (`TERM=dumb`, `NO_COLOR=1`, monochrome terminals) or for color-blind users, zero structural information is lost.

---

## 2. State & Glyph Matrix

| State | Glyph | Visual Bar Representation (20 cells) | Semantic Meaning & Informational Text |
|---|---|---|---|
| **Done** | `●` | `████████████████████` (full, dim) | Reported compilation time and heap allocation. |
| **Compiling** | `◐` | `████████▌             ` (phase segments) | Elapsed observed time against statistical estimate. |
| **Overrun** | `◐` | `████████████████████` (overrun highlight) | Exceeded statistical median duration (`+Xs vs usual`). |
| **No History** | `◐` | `  ██████            ` (animated sweep) | Compiling module with no prior recorded history. |
| **Waiting** | `○` | `····················` (tick per import) | Pending module, ticks show completed dependencies. |
| **Skipped** | `◇` | `◇ ◇ ◇ ◇ ◇ ◇ ◇ ◇ ◇ ◇ ` | Cached or unchanged module (`interface unchanged`). |
| **Folded Group**| `▸` | `████████████        ` (stacked) | Collapsed subtree showing done/compiling counts. |
| **Failed** | `✖` | None (count only) | Fatal compilation failure (`✖ n errors`). |
| **Blocked** | `⊘` | `╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳╳` (hatched) | Unreached due to upstream dependency failure. |
| **Not Reached** | `◌` | `····················` (dotted) | Target aborted before compilation was attempted. |

---

## 3. Progress Bar Geometry (20 Cells)

### 3.1 Cell Geometry
All progress bars in `buildr` conform to a fixed geometric budget of **20 character cells**, right-aligned before the module diagnostic/info column.

### 3.2 Fractional Block Glyph Resolution
To render smooth sub-character progress, fractional cells use Unicode eighth-block characters:

```
  (empty)
▏ (1/8)
▎ (2/8)
▍ (3/8)
▌ (4/8)
▋ (5/8)
▊ (6/8)
▉ (7/8)
█ (8/8 full)
```

The fractional character is chosen via:
$$\text{remainder} = \lfloor 8 \times (\text{fractional remainder}) \rfloor$$

---

## 4. Layout Rules & View Modes

### 4.1 Tree Truncation & Frontier Priority
1. **Dynamic Row Allocation**: Terminal height minus header and summary block ($H - 8$).
2. **Deepest First Folding**: Deep dependency subtrees are folded first into `▸` group nodes.
3. **Frontier Invariant**: Active compiling modules (`◐`) are NEVER folded; they remain pinned in the active frontier view.

### 4.2 Error Formatting & Scrollback Cleanliness
1. Errors print **above** the active live region.
2. In-progress live frames are rewritten in place using `\ESC[K` line clear sequences.
3. The final exit frame preserves an honest, linear scrollback history with a deterministic summary block.

### 4.3 Standard Views
* **Graph View** (Default): Visual dependency frontier and active compiling units.
* **Zen View** (One-line / Narrow): Minimalist single-line progress indicator, activated dynamically when rows $< 10$.
* **Mosaic View**: High-density multi-column module grid for large monitors.
* **Plain / CI View**: Non-interactive sequential stream formatting with GitHub Actions `::group::` folds.
* **Linear View**: Accessible, non-redraw sequential stream formatted for screen readers.
