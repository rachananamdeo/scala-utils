package com.shanker.annotations

import scala.annotation.StaticAnnotation
import scala.annotation.meta._

/**
 * 
 */

@field @getter @setter @beanGetter @beanSetter
class Since(version:String) extends StaticAnnotation