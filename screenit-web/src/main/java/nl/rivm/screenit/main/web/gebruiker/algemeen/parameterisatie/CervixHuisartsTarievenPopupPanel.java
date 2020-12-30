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
import java.util.Date;

import nl.rivm.screenit.dao.cervix.CervixVerrichtingDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.validator.BigDecimalPuntFormValidator;
import nl.rivm.screenit.model.cervix.facturatie.CervixHuisartsTarief;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.DateValidator;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class CervixHuisartsTarievenPopupPanel extends GenericPanel<CervixHuisartsTarief>
{
	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixVerrichtingDao cervixVerrichtingDao;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	public CervixHuisartsTarievenPopupPanel(String id)
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
		startDatumDatePicker.add(DateValidator.minimum(currentDateSupplier.getDateTimeMidnight().plusDays(1).toDate()));
		startDatumDatePicker.setLabel(Model.of("Geldig vanaf"));
		form.add(startDatumDatePicker);

		form.add(new AjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				CervixHuisartsTarief tarief = (CervixHuisartsTarief) form.getModelObject();
				if (cervixVerrichtingDao.getCervixHuisartsTarief(tarief.getGeldigVanafDatum()) != null)
				{
					error("Er is al een tarief met dezelfde geldig vanaf datum, selecteer een andere datum.");
				}
				else
				{
					cervixVerrichtingService.toevoegenTarief(tarief, ScreenitSession.get().getLoggedInAccount());
					opslaan(target);
				}
			}
		});

		add(form);

	}

	protected abstract void opslaan(AjaxRequestTarget target);
}
