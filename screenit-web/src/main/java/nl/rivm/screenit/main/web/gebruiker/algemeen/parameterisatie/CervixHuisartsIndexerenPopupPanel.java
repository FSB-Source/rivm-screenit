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
import nl.rivm.screenit.main.web.gebruiker.gedeeld.cervix.CervixHerindexeringWaarschuwingPanel;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.DependantDateValidator;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class CervixHuisartsIndexerenPopupPanel extends GenericPanel<CervixHuisartsTarief>
{
	@SpringBean
	private CervixVerrichtingService verrichtingService;

	@SpringBean
	private CervixBetalingService betalingService;

	public CervixHuisartsIndexerenPopupPanel(String id)
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

		form.add(new CervixHerindexeringWaarschuwingPanel("waarschuwing"));

		var tariefField = new BigDecimalField("tarief");
		tariefField.setRequired(true);
		form.add(tariefField);
		form.add(new BigDecimalPuntFormValidator(tariefField));

		var startDatumDatePicker = ComponentHelper.newDatePicker("geldigVanafDatum");
		startDatumDatePicker.setRequired(true);
		startDatumDatePicker.setLabel(Model.of("Geldig van"));
		form.add(startDatumDatePicker);

		var eindDatumDatePicker = ComponentHelper.newDatePicker("geldigTotenmetDatum");
		eindDatumDatePicker.setLabel(Model.of("Geldig t/m"));
		form.add(eindDatumDatePicker);

		form.add(new DependantDateValidator(startDatumDatePicker, eindDatumDatePicker, DependantDateValidator.Operator.AFTER));

		form.add(new IndicatingAjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				try
				{
					var nieuweTarief = (CervixTarief) HibernateHelper.deproxy(ModelProxyHelper.deproxy(form.getModelObject()));
					var melding = betalingService.toevoegenIndexatieTarief(nieuweTarief, ScreenitSession.get().getLoggedInAccount());
					opslaan(target, melding);
				}
				catch (IllegalArgumentException e)
				{
					error(getString(e.getMessage()));
				}

			}
		});

		add(form);

	}

	protected abstract void opslaan(AjaxRequestTarget target, String melding);
}
