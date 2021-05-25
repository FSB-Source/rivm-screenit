package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.verslag.laesies;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDto;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.dto.LaesieDtoMapper;
import nl.rivm.screenit.model.mamma.MammaLaesie;
import nl.rivm.screenit.service.mamma.MammaBaseLaesieService;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

class LaesieTypePanel extends GenericPanel<LaesieDto>
{
	@SpringBean
	private MammaBaseLaesieService laesieService;

	private List<LaesieDto> alleLaesies;

	private final LaesieDtoMapper mapper = new LaesieDtoMapper();

	LaesieTypePanel(String id, IModel<LaesieDto> model, List<LaesieDto> alleLaesies)
	{
		super(id, model);
		this.alleLaesies = alleLaesies;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(new Label("laesieType", getLaesieTypeEnEventueelVolgnummer()));
	}

	private String getLaesieTypeEnEventueelVolgnummer()
	{
		return getModelObject().getLaesietype().getNaam() + volgnummerPostfix() + " (" + StringUtils.capitalize(getModelObject().getWelkeBorst().getNaam()) + ")";
	}

	private String volgnummerPostfix()
	{
		LaesieDto laesie = getModelObject();
		MammaLaesie mammaLaesie = mapper.laesieDtoToMammaLaesie(laesie);
		final List<MammaLaesie> mammaLaesieList = alleLaesies.stream().map(mapper::laesieDtoToMammaLaesie).collect(Collectors.toList());
		return laesieService.isVolgnummerNodig(mammaLaesieList, mammaLaesie) ? " - " + laesie.getNummer() : "";
	}

}
