l = iface.activeLayer()

req1 = QgsFeatureRequest().setFilterExpression(u'"seccion" = 4608')
req2 = QgsFeatureRequest().setFilterExpression(u'"seccion" = 4995')
req3 = QgsFeatureRequest().setFilterExpression(u'"seccion" = 4627')
req4 = QgsFeatureRequest().setFilterExpression(u'"seccion" = 4607')

elem_fix = l.getFeatures(req1)
elem_1 = l.getFeatures(req2)
elem_2 = l.getFeatures(req3)
elem_3 = l.getFeatures(req4)

    ring_f = QgsGeometry.fromPolyline(f.geometry().asPolygon()[0])
    ring_intersecting = QgsGeometry.fromPolyline(intersecting_f.geometry().asPolygon()[0])
    ring_intersection = ring_f.intersection(ring_intersecting)
