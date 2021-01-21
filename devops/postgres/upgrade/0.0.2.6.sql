    CREATE TABLE public.ngrams_postag (
        id SERIAL,
        lang_id INTEGER,
        algo_id INTEGER,
        postag CHARACTER varying(5),
        ngrams_id INTEGER NOT NULL,
        lemm_id   INTEGER NOT NULL,
        score     INTEGER DEFAULT 1 ::integer NOT NULL,
        FOREIGN KEY (ngrams_id) REFERENCES public.ngrams(id) ON DELETE CASCADE,
        FOREIGN KEY (lemm_id)   REFERENCES public.ngrams(id) ON DELETE CASCADE
    )  ;
    -- ALTER TABLE public.ngrams_postag OWNER TO gargantua;

    CREATE UNIQUE INDEX ON public.ngrams_postag (lang_id,algo_id,postag,ngrams_id,lemm_id);


