# Lila Development Guide for Coding Agents

Before making any changes, read `AI_POLICY.md` and `CONTRIBUTING.md`.

## Repository Overview

Lila (li[chess in sca]la) is the free, open-source chess server powering lichess.org - one of the world's largest chess platforms with 12+ billion games. This is a production-scale application serving millions of users with real-time gameplay, computer analysis, tournaments, and comprehensive chess features.

**Technology Stack:**

- **Backend**: Scala 3.8.2 with Liplay 3.2 (Lichess Play Framework fork), SBT build system
- **Frontend**: TypeScript with Snabbdom, PNPM workspace monorepo
- **Database**: MongoDB with Elasticsearch indexing
- **Real-time**: WebSocket connections via separate lila-ws server, Redis for communication
- **Chess Engine**: Stockfish via distributed fishnet cluster
- **Styling**: Sass with custom build pipeline

## Environment Requirements

**CRITICAL**: These exact versions are required - the build will fail without them:

- **Java 21** (JDK, not JRE - needs jdk.compiler module)
- **Node.js 24.20+** (specified in `.node-version`)
- **PNPM 12+** (specified in `package.json`)

**Installation:**

```bash
# Install PNPM globally
npm install -g pnpm@12.9.0

# Install dependencies (always run this first)
pnpm install
```

**Nix development:** `devenv` is supported; its inputs and tools are configured in `devenv.yaml` and `devenv.nix`. Before bumping a tool version, confirm the required package/version is available in the selected Nixpkgs package set, or update the Nixpkgs input/configuration/lock as needed.

## Build & Development Commands

**Frontend (UI) Development:**

```bash
# Build all UI packages (requires Node 24+)
./ui/build

# Build specific packages
./ui/build site analyse lobby

# Watch mode for development
./ui/build -w

# Production build
./ui/build -p

# No dependency installation (faster rebuilds)
./ui/build --no-install
```

**Backend (Scala) Development:**

```bash
# Start development console (lila.sh is the SBT wrapper)
# Automatically copies .sbtopts.default and conf/application.conf.default if missing
./lila.sh

# In SBT console (via ./lila.sh), compile and run:
sbt> compile
sbt> run

# Run with specific JVM options
./lila.sh -Depoll=true

# Full build with tests and staging
./lila.sh "test;stage"
```

**Testing & Quality:**

```bash
# Run all frontend tests (Vitest)
pnpm test

# Watch mode for tests
pnpm test:watch

# Check code formatting (Oxfmt)
pnpm check-format

# Auto-format code
pnpm format

# Lint TypeScript code
pnpm lint

# Auto-fix lint issues
pnpm lint:fix

# Backend formatting check (Scalafmt) - via lila.sh wrapper
./lila.sh scalafmtCheckAll

# Auto-format Scala code
./lila.sh scalafmtAll
```

## Project Architecture

**Module Structure (80+ Scala modules):**

- Modules are in `/modules/[name]/src/main/` with strict dependency hierarchy
- Core modules: `core`, `common`, `db`, `memo`, `ui`
- Domain modules: `game`, `user`, `tournament`, `study`, `puzzle`, etc.
- Build order matters - see `build.sbt` for dependency graph

**Frontend Structure (36 UI packages):**

- `/ui/[package]/` - Individual TypeScript packages
- `/ui/lib/` - Shared utilities and types
- `/ui/@types/` - TypeScript definitions
- `/ui/.build/` - Custom build system with esbuild
- Each package has `package.json` with custom "build" property

**Key Directories:**

- `/app/` - Play Framework controllers, views, main application
- `/conf/` - Configuration files, routes, application settings
- `/public/` - Static assets, will contain compiled CSS/JS after build
- `/bin/` - Utility scripts (deploy, CLI tools, git hooks)
- `/project/` - SBT build configuration (BuildSettings.scala, Dependencies.scala)

## Common Issues & Solutions

**Node Version Error:** `Nodejs v24.20.0 or later is required`

- Install Node 24.20+ using `nvm`, `volta` or a package manager
- Check with: `node -v`

**Java Version Issues:**

- Ensure Java 21 JDK is installed (not JRE)
- Check: `java --list-modules | grep jdk.compiler`
- Set JAVA_HOME properly

**Build Failures:**

- Always run `pnpm install` after pulling changes
- For UI issues: `./ui/build --clean` then rebuild
- For Scala issues: `sbt clean compile`

**PNPM Workspace Issues:**

- Dependencies use `workspace:*` for internal packages
- Run `pnpm install` from repository root, not subdirectories

## Development Workflow

**Making Changes:**

1. **Frontend**: Edit files in `/ui/[package]/src/`, run `./ui/build -w` for live reload
2. **Backend**: Edit `/modules/[name]/src/main/`, use `sbt ~compile` for auto-recompile
3. **Styles**: Edit `.scss` files, included in UI build process
4. **Configuration**: Edit `/conf/` files, may require server restart

**Adding or updating i18n:**

- Edit source strings in `translation/source/*.xml`; then run `pnpm i18n-file-gen` and `./ui/build --i18n` to regenerate Scala keys and UI typings/translation files.
- Locale translations live in `translation/dest/` and are synchronized through Crowdin. Validate them with `./bin/trans-lint translation/dest/*/*.xml`.

**Before Committing:**

1. Run `pnpm check-format` and `pnpm lint`
2. Run `./lila.sh scalafmtCheckAll`
3. Run `pnpm test` for frontend tests
4. Consider `./lila.sh test` for affected backend modules (can be slow)

**Utility Scripts:**

- `/bin/deploy` - Production deployment automation
- `/bin/trans-lint` - Translation validation
- `/bin/git-hooks/` - Git hooks for formatting/linting

## GitHub Actions & CI

**Workflows validate:**

- **server.yml**: Scala compilation, tests, formatting with Java 21
- **assets.yml**: UI build, tests, formatting with Node 24+
- **lint.yml**: Code quality checks

**Deployment:**

- Uses `/bin/deploy` script with workflow artifacts
- Server builds create `lila-3.0.tar.zst` artifact
- Asset builds create `assets.tar.zst` artifact

## Key Configuration Files

- **build.sbt**: Main SBT configuration, module definitions
- **package.json**: Root package with scripts and dependencies
- **pnpm-workspace.yaml**: Defines workspace packages
- **ui/.build/**: Custom frontend build system
- **.scalafmt.conf**: Scala formatting rules
- **.oxlintrc.jsonc**: TypeScript formatting rules (Oxlint)
- **.oxfmtrc.json**: TypeScript/CSS/JSON/MD formatting rules (Oxfmt)
- **conf/routes**: HTTP route definitions
- **conf/application.conf.default**: Main application configuration template

## Development Tips

- **Large codebase**: Use specific module builds rather than full recompilation
- **Asset compilation**: The `./ui/build` system is sophisticated - study `/ui/README.md` for details
- **Hot reloading**: Backend changes require manual restart, frontend has watch mode
- **Module dependencies**: Check `build.sbt` before adding cross-module dependencies
- **Database**: Uses MongoDB - no migrations, but schema assumptions in code
- **Performance**: This is a high-traffic production system - consider performance impact
- **UI**:
  - Snabbdom views can use tag helpers from `lib/view/snabbdomElements` (for example, `div()` or `a('/path')`) instead of `h` or `hl`.
  - Snabbdom JSX syntax is also supported - import `jsx` from `lib/view`.

**Trust these instructions** - they are validated and comprehensive. Only search for additional information if these instructions are incomplete or incorrect for your specific task.

## Площадка «Логик»: свободный анализ

- В меню анализа доступны обычные шахматы и шахматы Фишера. Список задаётся
  в `modules/analyse/src/main/ui/AnalyseUi.scala` отдельно от вариантов в лобби.
- На экранах от 1260px доска свободного анализа стандартных шахмат и Фишера
  центрируется относительно экрана с учётом `---sidenav-w`. Правила находятся
  в `ui/analyse/css/_analyse.free.scss`; мобильная версия, встроенная доска,
  режим для незрячих и страницы разбора партий сохраняют свою разметку.
- Проверять оба пункта меню, переключение на Фишера, ход на доске, шкалу
  оценки, изменение размера доски и отсутствие горизонтальной прокрутки.
  Контрольные размеры: 1920×1080, 1536×864, 1366×768 и 390×844.
- Перед частичной выкаткой сверять боевые исходники и контрольные суммы
  заменяемых файлов, сохранять резервную копию. При обновлении манифеста
  ассетов сохранять записи остальных модулей: на сервере бывают отдельные
  исправления, ещё не вошедшие в общую сборку.

## Площадка «Логик»: игровая доска

- В `ui/round/css/_layout.scss` игровая доска центрируется относительно
  экрана от 1260px с учётом `---sidenav-w`. Информация о партии и чат остаются
  слева, часы и ходы справа. Правило действует и при просмотре партии.
- Проверять ширины 1920, 1536, 1366, 1280 и 1260px, включая максимальный размер
  доски; дополнительно 1024px, телефон 390px и телефон в альбомной ориентации.
  Проверять часы, список ходов, раскрытие меню и отсутствие горизонтальной
  прокрутки. Мобильную раскладку, режим для незрячих и стримеров не менять.
- Для этой правки достаточно опубликовать новый `round.<хеш>.css` и обновить
  запись `round` в обоих манифестах. Команда администратора `change asset version`
  применяет манифесты без перезапуска игрового сервера.

## Площадка «Логик»: доска задач

- В `ui/puzzle/css/_layout.scss` доска задач центрируется относительно экрана
  от 1260px, как доски игры и свободного анализа. Учитывать боковое меню,
  три зазора по 4px и компенсацию ширины шкалы оценки шириной панели решения.
- Проверять обычные задачи, задачу дня и серию задач: размеры 1920×1080,
  1536×864, 1366×768, 1280×900 и 1260×1080, в том числе максимальный масштаб.
  Дополнительно проверять 1024×768, 390×844, 844×390, меню доски и просмотр
  решения со шкалой оценки. Мобильную версию и режим для незрячих не менять.
- Публиковать только новый `puzzle.<хеш>.css`, сохраняя остальные записи обоих
  манифестов; применять командой `change asset version` без перезапуска сервера.

## Площадка «Логик»: доска обучения

- В `ui/learn/css/_layout.scss` центрировать только `.learn--run` от 1260px:
  список уроков слева, доска по центру экрана, задание справа; зазоры по 2rem.
  Длинные названия уроков должны переноситься в пределах панели задания.
  Карта уроков `.learn--map`, мобильная версия и слепой режим сохраняют раскладку.
- Проверять 1920×1080, 1536×864, 1366×768, 1280×900, 1260×1080, 1024×768,
  390×844 и 844×390, включая увеличенную доску и длинные названия уроков.
  В анонимном сеансе проверить вводный экран, ход ладьи e2–e7 первого урока,
  переход к следующему уровню и возврат к карте; прогресс учеников не менять.
- Публиковать только `learn.<хеш>.css` с обновлением обоих манифестов,
  применять `change asset version` без перезапуска игрового сервера.

## Площадка «Логик»: просмотр завершённых партий

- `ui/analyse/css/_analyse.round.scss` центрирует доску просмотра партии от
  1260px. Слева остаются сведения о партии и чат, справа ходы и оценка позиции.
  Шкала оценки компенсируется шириной панели, поэтому не сдвигает доску.
- Проверять обе стороны доски, размеры 1920×1080, 1536×864, 1366×768,
  1280×900, 1260×1080, 1024×768, 390×844 и 844×390, включая максимальный масштаб,
  перемотку ходов, базу дебютов и шкалу оценки. Прямой эфир `/tv` использует
  центрирование из `round`; дополнительно проверить его на компьютере и телефоне.
- Не менять раскладку студий, мобильной версии, слепого режима и крейзихауса.
  Публиковать только `analyse.round.<хеш>.css`, сохраняя остальные записи обоих
  манифестов; применять `change asset version` без перезапуска сервера.
