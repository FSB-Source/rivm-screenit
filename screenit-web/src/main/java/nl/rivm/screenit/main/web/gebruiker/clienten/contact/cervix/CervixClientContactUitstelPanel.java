package nl.rivm.screenit.main.web.gebruiker.clienten.contact.cervix;

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

import java.time.LocalDate;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.cervix.CervixScreeningRonde;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstel;
import nl.rivm.screenit.model.cervix.enums.CervixUitstelType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

public class CervixClientContactUitstelPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	private WebMarkupContainer detailContainer;

	private IModel<Client> clientModel;

	private IModel<CervixUitstel> uitstelModel;

	private IModel<Date> datumModel;

	private Integer uitstelBijZwangerschap;

	private LocalDate dagNaDertigsteVerjaardag;

	public CervixClientContactUitstelPanel(String id, IModel<ClientContactActie> model, IModel<Client> client, List<Object> extraPanelParams)
	{
		super(id, model);
		clientModel = client;

		Date geboortedatum = client.getObject().getPersoon().getGeboortedatum();
		dagNaDertigsteVerjaardag = DateUtil.toLocalDate(geboortedatum).plusYears(30).plusDays(1);

		uitstelBijZwangerschap = preferenceService.getInteger(PreferenceKey.UITSTEL_BIJ_ZWANGERSCHAP_CERVIX.name());

		CervixScreeningRonde ronde = client.getObject().getCervixDossier().getLaatsteScreeningRonde();
		CervixUitstel uitstel = ronde.getUitstel();
		if (uitstel == null)
		{
			uitstel = new CervixUitstel();
			uitstel.setScreeningRonde(ronde);
		}

		uitstelModel = ModelUtil.cModel(uitstel);
		datumModel = new Model<>(DateUtil.toUtilDate(getDatum()));

		WebMarkupContainer container = new WebMarkupContainer("container", uitstelModel);
		add(container);

		ScreenitDropdown<CervixUitstelType> uitstelType = new ScreenitDropdown<>("uitstelType", Arrays.asList(CervixUitstelType.values()), new EnumChoiceRenderer<>());
		uitstelType.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				WebMarkupContainer newDetailContainer = maakDetailContainer();
				detailContainer.replaceWith(newDetailContainer);
				detailContainer = newDetailContainer;
				target.add(detailContainer);
			}
		});
		container.add(uitstelType);

		detailContainer = maakDetailContainer();
		container.add(detailContainer);
	}

	private WebMarkupContainer maakDetailContainer()
	{
		WebMarkupContainer detailContainer = new WebMarkupContainer("detailContainer");
		detailContainer.setVisible(uitstelModel.getObject().getUitstelType() != null);
		detailContainer.setOutputMarkupPlaceholderTag(true);

		detailContainer.add(new EnumLabel<>("label", uitstelModel.getObject().getUitstelType()));
		DatePicker<Date> datum = new DatePicker<>("datum", datumModel, Date.class);
		datum.setRequired(true);
		detailContainer.add(datum);
		return detailContainer;
	}

	@Override
	public void validate()
	{
		super.validate();

		CervixUitstel uitstel = uitstelModel.getObject();

		if (uitstel.getUitstelType() == null)
		{
			error(getString("error.geen.uitstelType"));
		}
		else if (datumModel == null)
		{
			error(getString("error.geen.uitstellenToDatum"));
		}
		else
		{
			LocalDate datum = DateUtil.toLocalDate(datumModel.getObject());
			LocalDate morgen = dateSupplier.getLocalDate().plusDays(1);
			LocalDate uitstellenTotDatum = getUitstellenTotDatum();
			CervixUitstelType uitstelType = uitstel.getUitstelType();
			Client client = clientModel.getObject();
			LocalDate dagNaDertigsteVerjaardag = DateUtil.toLocalDate(client.getPersoon().getGeboortedatum()).plusYears(30).plusDays(1);

			if (uitstelType == CervixUitstelType.ZWANGERSCHAP && uitstellenTotDatum.isBefore(morgen))
			{
				error(String.format(getString("error.uitstellenToDatum.voor.morgen"), uitstelBijZwangerschap));
			}
			else if (uitstelType == CervixUitstelType.ZWANGERSCHAP && morgen.plusMonths(9).minusDays(1).isBefore(datum))
			{
				error(getString("error.uitstellenToDatum.meer.dan.negen.maanden"));
			}
			else if (uitstelType == CervixUitstelType.ZWANGERSCHAP && uitstellenTotDatum.isBefore(dagNaDertigsteVerjaardag))
			{
				error(String.format(getString("error.uitstel.voor.de.dertigste.verjaardag.zwanger"),
					dagNaDertigsteVerjaardag.minusDays(uitstelBijZwangerschap).format(DateUtil.LOCAL_DATE_FORMAT)));
			}
			else if (uitstelType == CervixUitstelType.ANDERS && uitstellenTotDatum.isBefore(morgen))
			{
				error(getString("error.uitstellenToDatum.in.toekomst"));
			}
			else if (uitstelType == CervixUitstelType.ANDERS && uitstellenTotDatum.isBefore(dagNaDertigsteVerjaardag))
			{
				error(getString("error.uitstel.voor.de.dertigste.verjaardag.anders"));
			}
			else if (uitstel.getScreeningRonde().getDossier().getVolgendeRondeVanaf() != null && uitstel.getScreeningRonde().getDossier().getVolgendeRondeVanaf()
				.before(DateUtil.toUtilDate(uitstellenTotDatum)))
			{
				error(getString("error.uitstel.loopt.af.na.volgende.ronde"));
			}
		}
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> meldingen = super.getOpslaanMeldingen();
		LocalDate uitstellenTotDatum = getUitstellenTotDatum();
		meldingen.add("Uitstel tot " + uitstellenTotDatum.format(DateUtil.LOCAL_DATE_FORMAT) + ".");

		CervixUitnodiging zasUitnodiging = uitstelModel.getObject().getScreeningRonde().getLaatsteZasUitnodiging();
		if (zasUitnodiging != null && zasUitnodiging.getVerstuurdDatum() == null && zasUitnodiging.getGeannuleerdDatum() == null)
		{
			meldingen.add("Aanvraag ZAS wordt geannuleerd.");
		}

		return meldingen;
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();
		CervixUitstel uitstel = uitstelModel.getObject();
		uitstel.setUitstellenTotDatum(DateUtil.toUtilDate(getUitstellenTotDatum()));
		opslaanObjecten.put(ExtraOpslaanKey.CERVIX_UITSTEL, uitstel);
		return opslaanObjecten;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(uitstelModel);
		ModelUtil.nullSafeDetach(clientModel);
		ModelUtil.nullSafeDetach(datumModel);
	}

	private LocalDate getUitstellenTotDatum()
	{
		LocalDate datum = DateUtil.toLocalDate(datumModel.getObject());
		LocalDate uitstellenTotDatum = null;
		switch (uitstelModel.getObject().getUitstelType())
		{
		case ZWANGERSCHAP:
			uitstellenTotDatum = datum.plusDays(uitstelBijZwangerschap);
			break;
		case ANDERS:
			uitstellenTotDatum = datum;
			break;
		}
		return uitstellenTotDatum;
	}

	private LocalDate getDatum()
	{
		CervixUitstel uitstel = uitstelModel.getObject();
		LocalDate datum = DateUtil.toLocalDate(uitstel.getUitstellenTotDatum());
		if (datum != null && uitstel.getUitstelType() == CervixUitstelType.ZWANGERSCHAP)
		{
			datum = datum.minusDays(uitstelBijZwangerschap);
		}
		return datum;
	}
}
