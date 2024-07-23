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

import java.math.BigDecimal;

import nl.rivm.screenit.main.service.cervix.CervixBetalingService;
import nl.rivm.screenit.main.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.validator.BigDecimalPuntFormValidator;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;

public abstract class CervixHuisartsTarievenPopupPanel extends GenericPanel<CervixHuisartsTarief>
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private CervixVerrichtingService verrichtingService;

	@SpringBean
	private CervixBetalingService betalingService;

	public CervixHuisartsTarievenPopupPanel(String id)
	{
		super(id, ModelUtil.ccModel(new CervixHuisartsTarief()));

		var tarief = getModelObject();
		var latest = verrichtingService.getLatestHuisartsTarief();
		if (latest != null)
		{
			tarief.setTarief(latest.getTarief());
		}
		else
		{
			tarief.setTarief(BigDecimal.ZERO);
		}

		var form = new Form<>("form", getModel());

		var tariefField = new BigDecimalField("tarief");
		tariefField.setRequired(true);
		form.add(tariefField);
		form.add(new BigDecimalPuntFormValidator(tariefField));

		var startDatumDatePicker = ComponentHelper.newDatePicker("geldigVanafDatum");
		startDatumDatePicker.setRequired(true);
		startDatumDatePicker.add(DateValidator.minimum(DateUtil.toUtilDate(currentDateSupplier.getLocalDate().plusDays(1))));
		startDatumDatePicker.setLabel(Model.of("Geldig vanaf"));
		form.add(startDatumDatePicker);

		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				var tarief = ModelProxyHelper.deproxy(form.getModelObject());
				if (verrichtingService.heeftHuisartsTarief(tarief.getGeldigVanafDatum()))
				{
					error("Er is al een tarief met dezelfde geldig vanaf datum, selecteer een andere datum.");
				}
				else
				{
					betalingService.toevoegenTarief(tarief, ScreenitSession.get().getLoggedInAccount());
					opslaan(target);
				}
			}
		});

		add(form);

	}

	protected abstract void opslaan(AjaxRequestTarget target);
}
