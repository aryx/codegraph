ifeq ($(FEATURE_VISUAL),1)
GTKINCLUDE=external/lablgtk2
CAIROINCLUDE=external/cairo2 external/cairo2-gtk
GUIDIRS=commons_wrappers/gui
endif

#------------------------------------------------------------------------------
# codegraph (was pm_depend)
#------------------------------------------------------------------------------
SYSLIBS_CG=$(SYSLIBS_CM)
OBJS_CG=code_graph/lib.cma

codegraph: $(LIBS) commons_wrappers/gui/lib.cma $(OBJS_CG) $(OBJS) main_codegraph.cmo
	$(OCAMLC) -thread $(CUSTOM) -o $@ $(SYSLIBS) threads.cma \
           $(SYSLIBS_CG) $(GTKLOOP) $^
codegraph.opt: $(LIBS:.cma=.cmxa) commons_wrappers/gui/lib.cmxa $(OBJS_CG:.cma=.cmxa) $(OPTOBJS) main_codegraph.cmx
	$(OCAMLOPT) -thread $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) threads.cmxa\
          $(SYSLIBS_CG:.cma=.cmxa) $(GTKLOOP:.cmo=.cmx)  $^

# far simpler dependencies
codegraph_build: $(LIBS) $(OBJS) main_codegraph_build.cmo
	$(OCAMLC) $(CUSTOM) -o $@ $(SYSLIBS) $^
codegraph_build.opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) main_codegraph_build.cmx
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa) $^
