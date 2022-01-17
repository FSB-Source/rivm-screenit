package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import nl.rivm.screenit.main.model.Parameterisatie;
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

	public TechnischeParametersPage() throws ParseException
	{
		Parameterisatie parameterisatie = parameterisatieService.loadParameterisatie();
		add(new TechnischeParametersPanel("parameters", new Model<>(parameterisatie)));

		IModel<IMSConfiguratie> imsConfiguratieModel = Model.of(parameterisatieService.getIMSConfiguratie());
		add(new TechnischBeheerIMSPanel("imsConfiguratie", imsConfiguratieModel));

		add(new TechnischBeheerXdsPanel("xdsParameters", new Model<>(parameterisatieService.loadParameterisatie())));

		add(new TechnischBeheerZorgIdPanel("zorgIdParameters", new Model<>(parameterisatieService.loadParameterisatie())));

		add(new TechnischBeheerOpenIDConnectPanel("openIdConnectParameters", new Model<>(parameterisatieService.loadParameterisatie())));

		add(new TechnischBeheerPostcodeNlPanel("postcodeNlParameters", new Model<>(parameterisatieService.loadParameterisatie())));

		add(new TechnischBeheerSopClassesPanel("sopClassesParameters", new Model<>(parameterisatieService.loadParameterisatie())));

		List<ColonUitnodigingsinterval> intervalParameters = new ArrayList<>(parameterisatieService.getIntervalParameters());
		Collections.sort(intervalParameters, new Comparator<ColonUitnodigingsinterval>()
		{
			@Override
			public int compare(ColonUitnodigingsinterval o1, ColonUitnodigingsinterval o2)
			{
				return Integer.valueOf(o1.getType().ordinal()).compareTo(Integer.valueOf(o2.getType().ordinal()));
			}
		});
		add(new TechnischeColonIntervalParameters("colonInterval", ModelUtil.listModel(intervalParameters, false)));

	}
}
