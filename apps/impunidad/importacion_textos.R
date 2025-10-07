### 1) Crear footers para todas las páginas ----

footer_superior <- 
  htmltools::HTML('<div class="footer">
<div class="footer-section contact-info">
  <img src="https://www.mexicoevalua.org/wp-content/uploads/2023/06/mxe-footer-logo-1-01.png" alt="Logo México Evalúa">
  <br>
    <p>México Evalúa es un centro de pensamiento y análisis que se enfoca en la evaluación y el monitoreo de la operación gubernamental para elevar la calidad de los resultados. Apoyamos los procesos de mejora de las políticas públicas a nivel federal, estatal y local mediante la generación y/o revisión de evidencia y la formulación de recomendaciones.</p>
  </div>
  <div class="footer-section contact-info">
  <br>
  <br>
  <br>
  <br>
  <br>
  <h4>CONTACTO</h3>
  <br>
  <p>Jaime Balmes No.11. Edificio D, 2o Piso. Despacho 202 Col. Los Morales Polanco. Deleg. Miguel Hidalgo C.P. 11510, CDMX</p>
  <p>Teléfono: <a href="tel:5559850252"> (55) 5985 0252</a><p>
  <p>Correo electrónico: <a class="footer-link"; href="mailto:info@mexicoevalua.org">info@mexicoevalua.org</a><p>
  </div>
  <div class="footer-section contact-info">
  </div>
  <div class="footer-section">
  </div>
                  </div>')

footer_inferior <- 
  htmltools::HTML('<div class="footer-inferior">
  <div class="contact-info">
  <p>© Copyright 2015 México Evalúa  <a class="footer-link"; href="https://www.mexicoevalua.org/aviso-de-privacidad">AVISO DE PRIVACIDAD</a></p>
  </div>
                  </div>')




### 2) Textos con diseño especial ---- 


txt_impunidad_variables <-
  htmltools::HTML('<p>
    El índice de impunidad considera las siguientes variables:
</p>
<ul style="list-style-type:square;">
    <li>
        <p style="line-height:1.2;margin-bottom:0pt;margin-left:40px;margin-top:0pt;text-align:justify;" dir="ltr">
            <span style="background-color:transparent;color:hsl(334,49%,37%);font-size:11pt;"><span style="-webkit-text-decoration-skip:none;font-style:normal;font-variant:normal;font-weight:400;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre-wrap;"><strong>Salidas alternas</strong></span></span><span style="background-color:transparent;color:#000000;font-size:11pt;"><span style="-webkit-text-decoration-skip:none;font-style:normal;font-variant:normal;font-weight:400;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre-wrap;"><strong>:</strong></span><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;"> Criterios de oportunidad <strong>+</strong> acuerdos reparatorios <strong>+</strong> suspensión condicional del proceso.</span></span>
        </p>
    </li>
    <li>
        <p style="line-height:1.2;margin-bottom:0pt;margin-left:40px;margin-top:0pt;text-align:justify;" dir="ltr">
            <span style="background-color:transparent;color:hsl(334,49%,37%);font-size:11pt;"><span style="-webkit-text-decoration-skip:none;font-style:normal;font-variant:normal;font-weight:400;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre-wrap;"><strong>Sentencias:</strong></span></span><span style="background-color:transparent;color:#000000;font-size:11pt;"><span style="-webkit-text-decoration-skip:none;font-style:normal;font-variant:normal;font-weight:400;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre-wrap;"><strong> </strong>en p</span><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;">rocedimiento abreviado y en juicio oral.</span></span>
        </p>
    </li>
    <li>
        <p style="line-height:1.2;margin-bottom:0pt;margin-left:40px;margin-top:0pt;text-align:justify;" dir="ltr">
            <span style="background-color:transparent;color:hsl(334,49%,37%);font-size:11pt;"><span style="-webkit-text-decoration-skip:none;font-style:normal;font-variant:normal;font-weight:400;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre-wrap;"><strong>Casos remitidos:</strong></span></span><span style="background-color:transparent;color:#000000;font-size:11pt;"><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;"> considera las C.I. iniciadas en 2024 y las pendientes de finalizar en 2023.</span></span>
        </p>
    </li>
    <li>
        <p style="line-height:1.2;margin-bottom:0pt;margin-left:40px;margin-top:0pt;text-align:justify;" dir="ltr">
            <span style="background-color:transparent;color:hsl(334,49%,37%);font-size:11pt;"><span style="-webkit-text-decoration-skip:none;font-style:normal;font-variant:normal;font-weight:400;text-decoration-skip-ink:none;vertical-align:baseline;white-space:pre-wrap;"><strong>Desestimaciones</strong></span><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;"><strong>:</strong></span></span><span style="background-color:transparent;color:#000000;font-size:11pt;"><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;"> Facultad de abstenerse de investigar <strong>+</strong> incompetencias <strong>+</strong> no ejercicio de la acción penal <strong>+</strong> acumulación <strong>+ </strong>sobreseimientos.</span></span>
        </p>
    </li>
</ul>
<p style="line-height:1.2;margin-bottom:0pt;margin-top:0pt;text-align:justify;" dir="ltr">
    Y se calcula siguiendo esta formula:
</p>
<figure class="image" data-ckbox-resource-id="H8Rjdhn4ytDT">
    <picture><source srcset="https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/150.webp 150w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/300.webp 300w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/450.webp 450w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/600.webp 600w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/750.webp 750w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/900.webp 900w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/1050.webp 1050w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/1200.webp 1200w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/1350.webp 1350w,https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/1498.webp 1498w" sizes="(max-width: 1498px) 100vw, 1498px" type="image/webp"><img src="https://ckbox.cloud/3ab5721130b019167fe6/assets/H8Rjdhn4ytDT/images/1498.png" width="1498" height="173"></picture>
</figure>')

txt_impunidad_intro <- 
  htmltools::HTML('<p>
    El índice de impunidad general del fuero común tuvo una <span style="color:hsl(334, 49%, 37%);font-size:26px;"><strong>media nacional de 97.2%</strong></span><span style="color:hsl(334, 49%, 37%);">,</span> esto significa que <span style="background-color:#ffe0b2;"><strong>menos del 3% de los casos alcanzaron una solución</strong></span>, aún y cuando se desestimaron los casos que no podrían alcanzar una solución.
</p>')


txt_costos_intro2 <- 
  htmltools::HTML('Los recursos en el sector de justicia son limitados, por lo que las instituciones deben constantemente balancear sus prioridades y tomar decisiones difíciles basadas 
                          en sus presupuestos.')

txt_costos_intro <-
  htmltools::HTML('<p>
    <span style="background-color:transparent;color:#000000;font-family:Calibri,sans-serif;font-size:18px;"><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;">El costo promedio es el resultado de </span></span><span style="background-color:#ffe0b2;color:#000000;font-family:Calibri,sans-serif;font-size:18px;"><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;"><strong>dividir el presupuesto total anual de una institución entre su carga de trabajo anual</strong></span></span><span style="background-color:transparent;color:#000000;font-family:Calibri,sans-serif;font-size:18px;"><span style="font-style:normal;font-variant:normal;font-weight:400;text-decoration:none;vertical-align:baseline;white-space:pre-wrap;"><strong>, </strong>ya sea medido en carpetas de investigación, causas penales o audiencias.&nbsp;Esta medida descansa en el supuesto de que el objetivo principal de las Fiscalías y los Tribunales es [….], por lo tanto, su prepuesto se podría dividir en su carga de trabajo para saber cuanto se le asigna en promedio a sus outputs.</span></span>
</p>')


txt_costos_tabla <- 
  htmltools::HTML('<p>
    No existe un costo promedio ideal o estándar para México, pero esta medida abre muchas conversaciones interesantes:
</p>
<figure class="table" style="width:60.36%;">
    <table class="ck-table-resized" style="border:1.5px solid hsl(0, 0%, 0%);">
        <colgroup><col style="width:40.96%;"><col style="width:59.04%;"></colgroup>
        <tbody>
            <tr>
                <td style="background-color:hsl(19, 43%, 83%);border-color:hsl(0, 0%, 60%);">
                    <strong>¿Un costo promedio bajo significa una gestión eficiente de los recursos?</strong>
                </td>
                <td>
                    <strong>No necesariamente</strong>, en la mayoría de los casos un costo promedio bajo está causado por un bajo presupuesto y una carga de trabajo alta.&nbsp;
                </td>
            </tr>
            <tr>
                <td style="background-color:hsl(19, 43%, 83%);">
                    <strong>¿Es necesario reducir los costos promedios altos?</strong>
                </td>
                <td>
                    <strong>No</strong>. Idealmente, las instituciones de justicia tienen los recursos suficientes para gestionar debidamente su carga de trabajo. Por ejemplo, pensando esta medida en el sector salud, ¿los recursos invertidos en cada paciente deberían ser lo más bajo posible? No.
                </td>
            </tr>
            <tr>
                <td style="background-color:hsl(19, 43%, 83%);">
                    <strong>Otra pregunta</strong>
                </td>
                <td>
                    Otra respuesta
                </td>
            </tr>
        </tbody>
    </table>
</figure>')

txt_capacidad_intro <- 
  htmltools::HTML('<p>
    El índice de capacidad pondera la efectividad y recursos de <span style="background-color:#ffe0b2;">cuatro instituciones claves</span> del SJP:
</p>
<ul style="list-style-type:circle;">
    <li>
        <span style="color:hsl(334,49%,37%);font-size:20px;"><strong>Fiscalía</strong></span>
    </li>
    <li>
        <span style="color:hsl(334,49%,37%);font-size:20px;"><strong>Poder Judicial</strong></span>
    </li>
    <li>
        <span style="color:hsl(334,49%,37%);font-size:20px;"><strong>Defensoría Pública</strong></span>
    </li>
    <li>
        <span style="color:hsl(334,49%,37%);font-size:20px;"><strong>Órgano de Coordinación</strong></span>
    </li>
</ul>')



img_derechos <- 
  htmltools::HTML('<figure class="image" data-ckbox-resource-id="Fk5OJjrXYJtP">
    <picture><source srcset="https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/222.webp 222w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/444.webp 444w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/666.webp 666w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/888.webp 888w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/1110.webp 1110w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/1332.webp 1332w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/1554.webp 1554w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/1776.webp 1776w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/1998.webp 1998w,https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/2220.webp 2220w" sizes="(max-width: 2220px) 100vw, 2220px" type="image/webp"><img src="https://ckbox.cloud/4e4e08aefc983ff99244/assets/Fk5OJjrXYJtP/images/2220.png" width="2220" height="1248"></picture>
</figure>')
