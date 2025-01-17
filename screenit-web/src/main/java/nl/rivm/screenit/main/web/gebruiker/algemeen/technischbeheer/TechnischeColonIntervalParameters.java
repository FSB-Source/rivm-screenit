package nl.rivm.screenit.main.web.gebruiker.algemeen.technischbeheer;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval;
import nl.rivm.screenit.model.colon.enums.ColonUitnodigingsintervalType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.IntervalEenheidAanduiding;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.colon.ColonDossierBaseService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class TechnischeColonIntervalParameters extends GenericPanel<List<ColonUitnodigingsinterval>>
{
	@SpringBean
	private ParameterisatieService parameterisatieService;

	@SpringBean
	private ColonDossierBaseService dossierBaseService;

	public TechnischeColonIntervalParameters(String id, IModel<List<ColonUitnodigingsinterval>> model)
	{
		super(id, model);
		Form<?> intervalForm = new Form<>("intervalForm");

		ListView<ColonUitnodigingsinterval> intervals = new ListView<ColonUitnodigingsinterval>("intervals", model)
		{
			@Override
			protected void populateItem(ListItem<ColonUitnodigingsinterval> item)
			{
				item.setModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new EnumLabel<ColonUitnodigingsintervalType>("type"));
				ComponentHelper.addTextField(item, "aantal", false, 4, Integer.class, !magAanpassen());
				ComponentHelper.addDropDownChoice(item, "eenheid", false, Arrays.asList(IntervalEenheidAanduiding.values()), !magAanpassen());
			}
		};

		intervalForm.add(intervals);
		add(intervalForm);
		AjaxSubmitLink parametersOpslaan = new AjaxSubmitLink("parametersOpslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<ColonUitnodigingsinterval> parameters = model.getObject();
				if (valideParameters(parameters))
				{
					parameterisatieService.saveColonIntervalParameters(parameters);
					dossierBaseService.updateIntervalReferentieDatums();
					info("Parameters zijn opgeslagen.");
				}
			}
		};
		parametersOpslaan.setVisible(magAanpassen());
		intervalForm.add(parametersOpslaan);
	}

	private boolean valideParameters(List<ColonUitnodigingsinterval> parameters)
	{
		final boolean[] valide = { true };
		parameters.forEach(parameter ->
		{
			if (parameter.getAantal() != null && parameter.getAantal() == 0)
			{
				parameter.setAantal(null);
			}
			if (parameter.getAantal() == null && !IntervalEenheidAanduiding.GEEN.equals(parameter.getEenheid()) ||
				parameter.getAantal() != null && IntervalEenheidAanduiding.GEEN.equals(parameter.getEenheid()))
			{
				String aantalDisplay = parameter.getAantal() == null ? "leeg" : parameter.getAantal().toString();
				error(String.format("Invalide parameter combinatie voor: \"%s\" [aantal: %s, eenheid: %s]",
					parameter.getType().naam(), aantalDisplay, parameter.getEenheid().getNaam()));
				valide[0] = false;
			}
		});
		return valide[0];
	}

	protected boolean magAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.TECHNISCH_BEHEER, Actie.AANPASSEN);
	}
}
