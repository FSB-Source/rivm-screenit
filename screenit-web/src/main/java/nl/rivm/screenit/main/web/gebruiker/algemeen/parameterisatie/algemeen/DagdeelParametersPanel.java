package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.algemeen;

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

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Date;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.model.Parameterisatie;
import nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie.BasePrimaireParametersPanel;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.input.timefield.TimeField;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;

public class DagdeelParametersPanel extends BasePrimaireParametersPanel
{
	private Model<Date> startMiddagDateModel;

	private Model<Date> startAvondDateModel;

	public DagdeelParametersPanel(String id, IModel<Parameterisatie> model)
	{
		super(id, model);
	}

	@Override
	protected Form<Parameterisatie> createAndGetForm()
	{
		Form<Parameterisatie> form = new Form<>("form");

		var dummyDatum = LocalDate.of(2022, 01, 01);

		var startMiddagLocalTime = (LocalTime) getModelObject().getParameters().get(PreferenceKey.START_MIDDAG);
		startMiddagDateModel = Model.of(DateUtil.toUtilDate(startMiddagLocalTime, dummyDatum));

		var startAvondLocalTime = (LocalTime) getModelObject().getParameters().get(PreferenceKey.START_AVOND);
		startAvondDateModel = Model.of(DateUtil.toUtilDate(startAvondLocalTime, dummyDatum));

		var startMiddagField = new TimeField("startMiddag", startMiddagDateModel, true).setRequired(true);
		var startAvond = new TimeField("startAvond", startAvondDateModel, true).setRequired(true);

		form.add(startMiddagField);
		form.add(startAvond);
		form.add(new DependantDateValidator(startMiddagField, startAvond, DependantDateValidator.Operator.AFTER));

		return form;
	}

	@Override
	protected Component createAndGetOpslaanLink()
	{
		return new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				getModelObject().getParameters().put(PreferenceKey.START_MIDDAG, DateUtil.toLocalTime(startMiddagDateModel.getObject()));
				getModelObject().getParameters().put(PreferenceKey.START_AVOND, DateUtil.toLocalTime(startAvondDateModel.getObject()));
				opslaan(target, Bevolkingsonderzoek.CERVIX, Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA);
				info("Dagdelen opgeslagen");
			}
		};
	}
}
