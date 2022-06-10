## Token tags for unknown, begin of sentence, end of sentence
TOKEN_BOS = "__bos__"
TOKEN_UNK = "__unk__"
TOKEN_EOS = "__eos__"
board_folder = glue::glue("{Sys.getenv('PIN_LOCAL_FOLDER')}capstone")
board <- pins::board_folder(board_folder, versioned = TRUE)
raw_data_dir <- paste0(here::here(), "/../capstone raw data/")
en_data_dir <- paste0(raw_data_dir, "final/en_US")
clean_files_dir <- paste0(raw_data_dir, "final/en_US/clean")
out_data_dir <- paste0(en_data_dir, '/', 'output_csvs')
inter_data_dir <- paste0(raw_data_dir, "final/en_US/intermediate")
model_data_dir <- paste0(en_data_dir, "/models/")
sql_data_dir <- paste0(en_data_dir,"/sql/")
options(parallelly.debug = TRUE)
future::plan(future::multisession(workers = 4))
dummy <- value(lapply(seq_len(nbrOfWorkers()), FUN = function(ii) {
  future(NULL, packages = "data.table", earlySignal = TRUE)
}))


KENLM_DIR = '/home/alex/bin/kenlm/'

KENLM_EXEC = '/home/alex/bin/kenlm/lmplz'

KENLM_QUERY = '/home/alex/bin/kenlm/query'
