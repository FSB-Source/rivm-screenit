package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.main.model.mamma.IMSConfiguratie;
import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TechnischeParametersPage extends TechnischBeheerPage
{

	@SpringBean
	private ParameterisatieService parameterisatieService;

	public TechnischeParametersPage()
	{
		var parameterisatie = parameterisatieService.loadParameterisatie();
		add(new TechnischeParametersPanel("parameters", new Model<>(parameterisatie)));

		IModel<IMSConfiguratie> imsConfiguratieModel = Model.of(parameterisatieService.getIMSConfiguratie());
		add(new TechnischBeheerIMSPanel("imsConfiguratie", imsConfiguratieModel));

		add(new TechnischBeheerXdsPanel("xdsParameters", new Model<>(parameterisatie)));

		add(new TechnischBeheerZorgIdPanel("zorgIdParameters", new Model<>(parameterisatie)));

		add(new TechnischBeheerOpenIDConnectPanel("openIdConnectParameters", new Model<>(parameterisatie)));

		add(new TechnischBeheerPostcodeNlPanel("postcodeNlParameters", new Model<>(parameterisatie)));

		add(new TechnischBeheerSopClassesPanel("sopClassesParameters", new Model<>(parameterisatie)));

		add(new TechnischBeheerSeParametersPanel("seSocketParameters", new Model<>(parameterisatie)));

		List<ColonUitnodigingsinterval> colonIntervalParameters = new ArrayList<>(parameterisatieService.getColonIntervalParameters());
		colonIntervalParameters.sort(Comparator.comparing(o -> o.getType().ordinal()));
		add(new TechnischeColonIntervalParameters("colonInterval", ModelUtil.listModel(colonIntervalParameters, false)));
	}
}
