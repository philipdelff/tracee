# lwrite with structure

    c("data_run001.csv", "data_run001.rds", "data_run001_meta.txt", 
    "ft_run001.png", "plot_hp_run001.png", "plot_wt_run001.png")
    

# lwrite with structure including script

    c("data_myscript.csv", "data_myscript.rds", "data_myscript_meta.txt", 
    "ft_myscript.png", "plot_hp_myscript.png", "plot_wt_myscript.png"
    )

# lwrite with custom structure

    c("mysc/data_run001.csv", "mysc/data_run001.rds", "mysc/data_run001_meta.txt", 
    "mysc/ft_run001.png", "mysc/plot_hp_run001.png", "mysc/plot_wt_run001.png"
    )

# lwrite with custom structure 2

    c("myscript/data_mod1.csv", "myscript/data_mod1.rds", "myscript/data_mod1_meta.txt", 
    "myscript/ft_mod1.png", "myscript/plot_hp_mod1.png", "myscript/plot_wt_mod1.png"
    )

# lwrite with custom structure 3

    c("scr1/data_run001.csv", "scr1/data_run001.rds", "scr1/data_run001_meta.txt", 
    "scr1/ft_run001.png", "scr1/plot_hp_run001.png", "scr1/plot_wt_run001.png"
    )

# nested lists - lists.as.subdirs

    c("myscript/list1/data_run001.csv", "myscript/list1/data_run001.rds", 
    "myscript/list1/data_run001_meta.txt", "myscript/list1/ft_run001.png", 
    "myscript/list1/plot_hp_run001.png", "myscript/list1/plot_wt_run001.png", 
    "myscript/list2/data_run001.csv", "myscript/list2/data_run001.rds", 
    "myscript/list2/data_run001_meta.txt", "myscript/list2/ft_run001.png", 
    "myscript/list2/plot_hp_run001.png", "myscript/list2/plot_wt_run001.png"
    )

# nested lists - lists.as.subdirs=FALSE

    c("myscript/list1_data_run001.csv", "myscript/list1_data_run001.rds", 
    "myscript/list1_data_run001_meta.txt", "myscript/list1_ft_run001.png", 
    "myscript/list1_plot_hp_run001.png", "myscript/list1_plot_wt_run001.png", 
    "myscript/list2_data_run001.csv", "myscript/list2_data_run001.rds", 
    "myscript/list2_data_run001_meta.txt", "myscript/list2_ft_run001.png", 
    "myscript/list2_plot_hp_run001.png", "myscript/list2_plot_wt_run001.png"
    )

