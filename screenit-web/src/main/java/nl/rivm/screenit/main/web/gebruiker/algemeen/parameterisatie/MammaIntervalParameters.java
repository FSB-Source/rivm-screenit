package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

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

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.service.ParameterisatieService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaUitnodigingsinterval;
import nl.rivm.screenit.model.mamma.enums.MammaUitnodigingsintervalType;
import nl.rivm.screenit.service.mamma.MammaVolgendeUitnodigingService;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.RangeValidator;

import static nl.rivm.screenit.main.web.base.BasePage.markeerFormulierenOpgeslagen;

public class MammaIntervalParameters extends GenericPanel<List<MammaUitnodigingsinterval>>
{
	@SpringBean
	private ParameterisatieService parameterisatieService;

	@SpringBean
	private MammaVolgendeUitnodigingService volgendeUitnodigingService;

	private Map<MammaUitnodigingsintervalType, Integer> oudeParameters;

	public MammaIntervalParameters(String id, IModel<List<MammaUitnodigingsinterval>> model)
	{
		super(id, model);
		var intervalForm = new Form<>("intervalForm");
		setOudeParameters();

		var intervals = new ListView<>("intervals", model)
		{
			@Override
			protected void populateItem(ListItem<MammaUitnodigingsinterval> item)
			{
				item.setModel(new CompoundPropertyModel<>(item.getModel()));
				item.add(new EnumLabel<MammaUitnodigingsintervalType>("type"));
				item.add(new TextField<>("aantalMaanden", Integer.class).setRequired(true).add(RangeValidator.range(1, 1200)).setEnabled(magAanpassen()));
			}
		};

		intervalForm.add(intervals);
		add(intervalForm);
		var parametersOpslaan = new AjaxSubmitLink("parametersOpslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<MammaUitnodigingsinterval> nieuweParameters = model.getObject();
				parameterisatieService.saveMammaIntervalParameters(nieuweParameters, oudeParameters, ScreenitSession.get().getLoggedInAccount());
				volgendeUitnodigingService.updateIntervalReferentieDatums();
				setOudeParameters();
				markeerFormulierenOpgeslagen(target);
				info("Intervallen volgende uitnodiging zijn opgeslagen.");
			}
		};
		parametersOpslaan.setVisible(magAanpassen());
		intervalForm.add(parametersOpslaan);
	}

	private void setOudeParameters()
	{
		oudeParameters = getModelObject().stream().collect(Collectors.toMap(MammaUitnodigingsinterval::getType, MammaUitnodigingsinterval::getAantalMaanden));
	}

	protected boolean magAanpassen()
	{
		return ScreenitSession.get().checkPermission(Recht.GEBRUIKER_BEHEER_PARAMETERISATIE, Actie.AANPASSEN);
	}
}
