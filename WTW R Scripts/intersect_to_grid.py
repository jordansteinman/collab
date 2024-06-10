import arcpy
arcpy.env.workspace = "C:/Data/Prioritization/WheretToWork/NB"

# Get user input
grid = arcpy.GetParameterAsText(0)
layers = arcpy.GetParameterAsText(1).split(";")
fgdb = arcpy.GetParameterAsText(2)

for layer in layers:
  desc = arcpy.Describe(layer)
  name = desc.name
  arcpy.AddMessage("Intersecting: {}".format(name))
  arcpy.analysis.Intersect([grid, layer], "{}/{}".format(fgdb, name))
