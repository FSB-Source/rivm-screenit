package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.bmhklaboratorium;

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
import nl.rivm.screenit.main.util.EnumStringUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.SimpleStringResourceModel;
import nl.rivm.screenit.main.web.component.form.BigDecimalField;
import nl.rivm.screenit.main.web.component.validator.BigDecimalPuntFormValidator;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.cervix.enums.CervixTariefType;
import nl.rivm.screenit.model.cervix.facturatie.CervixLabTarief;
import nl.rivm.screenit.model.cervix.facturatie.CervixTarief;
import nl.rivm.screenit.service.cervix.CervixAsyncIndexatieService;
import nl.rivm.screenit.service.cervix.CervixVerrichtingService;
import nl.rivm.screenit.util.cervix.CervixTariefUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public abstract class CervixLaboratoriumIndexerenPopupPanel extends GenericPanel<CervixLabTarief>
{
	@SpringBean
	private CervixVerrichtingDao cervixVerrichtingDao;

	@SpringBean
	private CervixVerrichtingService cervixVerrichtingService;

	@SpringBean
	private CervixAsyncIndexatieService asyncIndexatieService;

	public CervixLaboratoriumIndexerenPopupPanel(String id)
	{
		super(id, ModelUtil.cModel(new CervixLabTarief()));

		BMHKLaboratorium instelling = (BMHKLaboratorium) ScreenitSession.get().getCurrentSelectedOrganisatie();
		CervixLabTarief tarief = getModelObject();
		tarief.setBmhkLaboratorium(instelling);

		CervixLabTarief latest = cervixVerrichtingDao.getLatestCervixLabTarief(instelling);
		if (latest != null)
		{
			tarief.setHpvAnalyseZasTarief(latest.getHpvAnalyseZasTarief());
			tarief.setHpvAnalyseUitstrijkjeTarief(latest.getHpvAnalyseUitstrijkjeTarief());
			tarief.setCytologieNaHpvUitstrijkjeTarief(latest.getCytologieNaHpvUitstrijkjeTarief());
			tarief.setCytologieNaHpvZasTarief(latest.getCytologieNaHpvZasTarief());
			tarief.setCytologieVervolguitstrijkjeTarief(latest.getCytologieVervolguitstrijkjeTarief());
		}
		else
		{
			tarief.setHpvAnalyseZasTarief(BigDecimal.ZERO);
			tarief.setHpvAnalyseUitstrijkjeTarief(BigDecimal.ZERO);
			tarief.setCytologieNaHpvUitstrijkjeTarief(BigDecimal.ZERO);
			tarief.setCytologieNaHpvZasTarief(BigDecimal.ZERO);
			tarief.setCytologieVervolguitstrijkjeTarief(BigDecimal.ZERO);
		}

		Form<CervixLabTarief> form = new Form<>("form", getModel());

		BigDecimalField hpvAnalyseUitstrijkjeTarief = new BigDecimalField("hpvAnalyseUitstrijkjeTarief");
		hpvAnalyseUitstrijkjeTarief.setRequired(true);
		form.add(hpvAnalyseUitstrijkjeTarief);
		hpvAnalyseUitstrijkjeTarief.setLabel(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(CervixTariefType.LAB_HPV_ANALYSE_UITSTRIJKJE)));

		BigDecimalField hpvAnalyseZasTarief = new BigDecimalField("hpvAnalyseZasTarief");
		hpvAnalyseZasTarief.setRequired(true);
		form.add(hpvAnalyseZasTarief);
		hpvAnalyseZasTarief.setLabel(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(CervixTariefType.LAB_HPV_ANALYSE_ZAS)));

		BigDecimalField cytologieNaHpvUitstrijkjeTarief = new BigDecimalField("cytologieNaHpvUitstrijkjeTarief");
		cytologieNaHpvUitstrijkjeTarief.setRequired(true);
		form.add(cytologieNaHpvUitstrijkjeTarief);
		cytologieNaHpvUitstrijkjeTarief.setLabel(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(CervixTariefType.LAB_CYTOLOGIE_NA_HPV_UITSTRIJKJE)));

		BigDecimalField cytologieNaHpvZasTarief = new BigDecimalField("cytologieNaHpvZasTarief");
		cytologieNaHpvZasTarief.setRequired(true);
		form.add(cytologieNaHpvZasTarief);
		cytologieNaHpvZasTarief.setLabel(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(CervixTariefType.LAB_CYTOLOGIE_NA_HPV_ZAS)));

		BigDecimalField cytologieVervolguitstrijkjeTarief = new BigDecimalField("cytologieVervolguitstrijkjeTarief");
		cytologieVervolguitstrijkjeTarief.setRequired(true);
		form.add(cytologieVervolguitstrijkjeTarief);

		cytologieVervolguitstrijkjeTarief.setLabel(new SimpleStringResourceModel(EnumStringUtil.getPropertyString(CervixTariefType.LAB_CYTOLOGIE_VERVOLGUITSTRIJKJE)));

		DatePicker<Date> startDatumDatePicker = ComponentHelper.newDatePicker("geldigVanafDatum");
		startDatumDatePicker.setRequired(true);
		startDatumDatePicker.setLabel(Model.of("Geldig vanaf"));
		form.add(startDatumDatePicker);

		DatePicker<Date> eindDatumDatePicker = ComponentHelper.newDatePicker("geldigTotenmetDatum");
		eindDatumDatePicker.setLabel(Model.of("Geldig t/m"));
		form.add(eindDatumDatePicker);

		form.add(new BigDecimalPuntFormValidator(hpvAnalyseUitstrijkjeTarief, hpvAnalyseZasTarief, cytologieNaHpvUitstrijkjeTarief, cytologieNaHpvZasTarief,
			cytologieVervolguitstrijkjeTarief));

		form.add(new AjaxSubmitLink("opslaan", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				List<CervixTarief> oudeTarieven = new ArrayList<>();
				CervixTarief nieuweTarief = (CervixLabTarief) form.getModelObject();
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
