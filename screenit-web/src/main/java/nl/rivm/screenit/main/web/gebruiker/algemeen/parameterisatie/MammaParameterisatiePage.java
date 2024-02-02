package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.mamma.MammaUitnodigingsinterval;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	recht = Recht.GEBRUIKER_BEHEER_PARAMETERISATIE,
	actie = Actie.INZIEN,
	level = ToegangLevel.REGIO,
	bevolkingsonderzoekScopes = Bevolkingsonderzoek.MAMMA,
	constraint = ShiroConstraint.HasPermission)
public class MammaParameterisatiePage extends ParameterisatieBasePage
{
	@SpringBean
	private ParameterisatieService parameterisatieService;

	public MammaParameterisatiePage()
	{
		Parameterisatie parameterisatie = parameterisatieService.loadParameterisatie();
		add(new MammaPrimaireParametersPanel("landelijkeParameters", new Model<>(parameterisatie)));

		List<MammaUitnodigingsinterval> intervalParameters = new ArrayList<>(parameterisatieService.getMammmaIntervalParameters());
		intervalParameters.sort(Comparator.comparing(o -> o.getType().ordinal()));
		add(new MammaIntervalParameters("interval", ModelUtil.listModel(intervalParameters, false)));
	}
}
