package nl.rivm.screenit.main.web.gebruiker.algemeen.parameterisatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.validator.BigDecimalPuntFormValidator;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixAsyncIndexatieService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class CervixHuisartsIndexerenPopupPanel extends GenericPanel<CervixHuisartsTarief>
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixVerrichtingDao cervixVerrichtingDao;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	@SpringBean
	private CervixAsyncIndexatieService asyncIndexatieService;

	public CervixHuisartsIndexerenPopupPanel(String id)
	{
		super(id, ModelUtil.cModel(new CervixHuisartsTarief()));

		CervixHuisartsTarief tarief = getModelObject();
		CervixHuisartsTarief latest = cervixVerrichtingDao.getLatestCervixHuisartsTarief();
		if (latest != null)
		{
			tarief.setTarief(latest.getTarief());
		}
		else
		{
			tarief.setTarief(BigDecimal.ZERO);
		}

		Form<CervixHuisartsTarief> form = new Form<>("form", getModel());

		BigDecimalField tariefField = new BigDecimalField("tarief");
		tariefField.setRequired(true);
		form.add(tariefField);
		form.add(new BigDecimalPuntFormValidator(tariefField));

		DatePicker<Date> startDatumDatePicker = ComponentHelper.newDatePicker("geldigVanafDatum");
		startDatumDatePicker.setRequired(true);
		startDatumDatePicker.setLabel(Model.of("Geldig vanaf"));
		form.add(startDatumDatePicker);

		DatePicker<Date> eindDatumDatePicker = ComponentHelper.newDatePicker("geldigTotenmetDatum");
		eindDatumDatePicker.setLabel(Model.of("Geldig t/m"));
		form.add(eindDatumDatePicker);

		form.add(new AjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<CervixTarief> oudeTarieven = new ArrayList<>();
				CervixHuisartsTarief nieuweTarief = (CervixHuisartsTarief) form.getModelObject();
				try
				{
					cervixVerrichtingService.toevoegenIndexatieTarief(nieuweTarief, oudeTarieven, ScreenitSession.get().getLoggedInAccount());
					String melding = "";
					for (CervixTarief oudeTarief : oudeTarieven)
					{
						if (!melding.isEmpty())
						{
							melding += "; ";
						}
						melding += CervixTariefUtil.getTariefString(oudeTarief);
						asyncIndexatieService.indexeren(oudeTarief.getId(), nieuweTarief.getId(), CervixTariefType.isHuisartsTarief(nieuweTarief));
					}
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
