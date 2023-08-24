package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.math.BigDecimal;

import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.model.mamma.enums.MammaLaesieType;
import nl.rivm.screenit.service.mamma.MammaLaesieLocatieService;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

class LaesieLocatiePanel extends GenericPanel<LaesieDto>
{
	@SpringBean
	private MammaLaesieLocatieService laesieTypeService;

	LaesieLocatiePanel(String id, IModel<LaesieDto> model)
	{
		super(id, model);
		LaesieDtoMapper mapper = new LaesieDtoMapper();
		MammaLaesie laesie = mapper.laesieDtoToMammaLaesie(getModelObject());
		WebMarkupContainer laesieGrootteContainer = new WebMarkupContainer("laesieGrootteContainer");
		BigDecimalField laesieGrootteInCm = new BigDecimalField("laesieGrootteInCm", 1, BigDecimal.valueOf(1, 1), BigDecimal.valueOf(100));
		laesieGrootteContainer.add(laesieGrootteInCm);
		laesieGrootteContainer.setVisible(!MammaLaesieType.ARCHITECTUURVERSTORING.equals(laesie.getMammaLaesieType()));
		add(laesieGrootteContainer);
		add(new Label("mammaZijde", StringUtils.capitalize(laesie.getMammaZijde().getNaam())));
		add(new Label("kwadrant", StringUtils.capitalize(laesieTypeService.laesie2kwadrant(laesie))));
		add(new Label("diepte", StringUtils.capitalize(laesieTypeService.laesie2diepte(laesie) + " een derde")));
	}

}
