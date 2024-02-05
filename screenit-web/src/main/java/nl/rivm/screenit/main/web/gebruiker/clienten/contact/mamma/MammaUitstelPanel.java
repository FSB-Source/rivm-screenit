
package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.validator.WerkdagValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.model.enums.BevestigingsType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.mamma.MammaUitstel;
import nl.rivm.screenit.service.mamma.MammaBaseUitstelService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaUitstelPanel extends AbstractClientContactActiePanel<MammaUitstel>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseUitstelService baseUitstelService;

	private IModel<Boolean> briefAanmaken = Model.of(true);

	private IModel<Boolean> clientContact = Model.of(false);

	private boolean isNieuweUitstel;

	private IModel<MammaStandplaatsPeriode> standplaatsPeriodeModel;

	public MammaUitstelPanel(String id, IModel<MammaUitstel> uitstel, boolean isNieuweUitstel, MammaStandplaatsPeriode standplaatsPeriode)
	{
		super(id, uitstel);
		this.isNieuweUitstel = isNieuweUitstel;

		this.standplaatsPeriodeModel = ModelUtil.sModel(standplaatsPeriode);
	}
	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		MammaUitstel uitstel = getModelObject();
		MammaStandplaats standplaats = uitstel.getStandplaats();
		String screeningsEenheidNaam = "";

		for (MammaStandplaatsRonde ronde : standplaats.getStandplaatsRonden())
		{
			for (MammaStandplaatsPeriode periode : ronde.getStandplaatsPerioden())
			{
				LocalDate standplaatsPeriodeVanaf = DateUtil.toLocalDate(periode.getVanaf());
				LocalDate standplaatsPeriodeTotEnMet = DateUtil.toLocalDate(periode.getTotEnMet());
				LocalDate streefDatum = DateUtil.toLocalDate(uitstel.getStreefDatum());
				if (!standplaatsPeriodeVanaf.isAfter(streefDatum)
					&& !standplaatsPeriodeTotEnMet.isBefore(streefDatum))
				{
					screeningsEenheidNaam = periode.getScreeningsEenheid().getNaam();
				}
			}
		}

		add(new Label("title", getString(isNieuweUitstel ? "nieuwe.uitstel" : "aanpassing.uitstel")));
		add(new Label("standplaats.naam"));

		add(new Label("screeningsEenheid", screeningsEenheidNaam));

		MammaStandplaatsLocatie locatie = standplaats.getLocatie();
		MammaStandplaatsLocatie tijdelijkeLocatie = standplaats.getTijdelijkeLocatie();
		if (tijdelijkeLocatie.getStartDatum() != null)
		{
			Date eindDatum = tijdelijkeLocatie.getEindDatum();
			eindDatum.setHours(23);
			eindDatum.setMinutes(59);
			Date uitstelDatum = uitstel.getStreefDatum();
			if (tijdelijkeLocatie.getStartDatum().compareTo(uitstelDatum) * uitstelDatum.compareTo(eindDatum) > 0)
			{
				locatie = tijdelijkeLocatie;
			}
		}

		add(new Label("adres", AdresUtil.getAdresVoorStandplaatsLocatie(locatie)));
		add(new WebMarkupContainer("tijdelijk").setVisible(locatie.getTijdelijk()));
		add(new Label("locatiebeschrijving", locatie.getLocatieBeschrijving()));

		add(ComponentHelper.newDatePicker("streefDatum").setRequired(true).add(new WerkdagValidator()));

		boolean vanuitPlanning = getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY).bkVanuitPlanning;
		Component clientContactCheckBox = ComponentHelper.newCheckBox("clientContact", clientContact).setVisible(vanuitPlanning);
		add(clientContactCheckBox);

		CheckBox briefAanmakenCheckBox = ComponentHelper.newCheckBox("briefAanmaken", briefAanmaken);
		add(briefAanmakenCheckBox);

		IndicatingAjaxLink<Void> wijzigMoment = new IndicatingAjaxLink<Void>("wijzigMoment")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				wijzigMoment(target);
			}
		};
		add(wijzigMoment);
	}

	@Override
	public void validate()
	{
		super.validate();
		boolean vanuitPlanning = getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY).bkVanuitPlanning;
		if (vanuitPlanning && !Boolean.TRUE.equals(clientContact.getObject()))
		{
			error(getString("geen.client.contact"));
		}

		MammaStandplaatsPeriode standplaatsPeriode = standplaatsPeriodeModel.getObject();
		MammaUitstel uitstel = getModelObject();

		String validatieError = baseUitstelService.valideerStandplaatsPeriode(standplaatsPeriode, DateUtil.toLocalDate(uitstel.getStreefDatum()));

		if (StringUtils.isNotBlank(validatieError))
		{
			error(validatieError);
		}
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		MammaUitstel uitstel = getModelObject();
		SimpleDateFormat dateFormat = new SimpleDateFormat("EEEE dd-MM-yyyy");
		return Arrays
			.asList(
				String.format("De afspraak wordt uitgesteld naar de streefdatum %s in %s", dateFormat.format(uitstel.getStreefDatum()),
					uitstel.getStandplaats().getNaam()));
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = new HashMap<>();

		opslaanObjecten.put(ExtraOpslaanKey.MAMMA_UITSTEL, ModelProxyHelper.deproxy(getModelObject()));
		if (Boolean.TRUE.equals(briefAanmaken.getObject()))
		{
			opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.BRIEF);
		}
		else
		{
			opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, BevestigingsType.GEEN);
		}
		return opslaanObjecten;
	}

	protected void wijzigMoment(AjaxRequestTarget target)
	{
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(standplaatsPeriodeModel);
	}
}
