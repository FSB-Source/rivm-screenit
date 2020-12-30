package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.ExtraOpslaanKey;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.CheckBox;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.joda.time.DateTime;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaAfspraakPanel extends AbstractClientContactActiePanel<MammaAfspraak>
{
	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService simplePreferenceService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	private IModel<Boolean> briefAanmaken = Model.of(false);

	private IModel<Boolean> uitnodigingsbriefTegenhouden = Model.of(false);

	private boolean isNieuweAfspraak;

	private boolean isGeforceerd;

	public MammaAfspraakPanel(String id, MammaAfspraak afspraak, boolean isNieuweAfspraak, boolean isGeforceerd)
	{
		super(id, ModelUtil.ccModel(afspraak));
		this.isNieuweAfspraak = isNieuweAfspraak;
		this.isGeforceerd = isGeforceerd;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		boolean isClientportaal = ScreenitSession.get().checkPermission(Recht.CLIENT_DASHBOARD, Actie.INZIEN);
		add(new Label("title", getString(isNieuweAfspraak ? "nieuwe.afspraak" : "huidige.afspraak")));
		add(new Label("standplaatsPeriode.standplaatsRonde.standplaats.naam").setVisible(!isClientportaal));
		add(new Label("capaciteitBlok.screeningsEenheid.naam").setVisible(!isClientportaal));

		MammaAfspraak afspraak = getModelObject();
		MammaStandplaats standplaats = afspraak.getStandplaatsPeriode().getStandplaatsRonde().getStandplaats();
		MammaStandplaatsLocatie locatie = standplaats.getLocatie();
		MammaStandplaatsLocatie tijdelijkeLocatie = standplaats.getTijdelijkeLocatie();
		if (tijdelijkeLocatie.getStartDatum() != null)
		{
			Date eindDatum = tijdelijkeLocatie.getEindDatum();
			eindDatum.setHours(23);
			eindDatum.setMinutes(59);
			Date vanaf = afspraak.getVanaf();
			if (tijdelijkeLocatie.getStartDatum().compareTo(vanaf) * vanaf.compareTo(eindDatum) > 0)
			{
				locatie = tijdelijkeLocatie;
			}
		}

		add(new Label("adres", AdresUtil.getAdresVoorStandplaatsLocatie(locatie)));
		add(new WebMarkupContainer("tijdelijk").setVisible(locatie.getTijdelijk()));
		add(new Label("locatiebeschrijving", locatie.getLocatieBeschrijving()).setVisible(isNieuweAfspraak || isClientportaal));

		add(DateLabel.forDatePattern("vanaf", "EEEE dd-MM-yyyy HH:mm"));

		CheckBox uitnodigingsbriefTegenhoudenCheckbox = ComponentHelper.newCheckBox("uitnodigingsbriefTegenhouden", uitnodigingsbriefTegenhouden);
		add(uitnodigingsbriefTegenhoudenCheckbox.setVisible(isNieuweAfspraak && isGeforceerd));

		CheckBox briefAanmakenCheckBox = ComponentHelper.newCheckBox("briefAanmaken", briefAanmaken);
		add(briefAanmakenCheckBox.setVisible(isNieuweAfspraak && !isGeforceerd));

		boolean vanuitPlanning = getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY).bkVanuitPlanning;

		WebMarkupContainer redenContainer = new WebMarkupContainer("redenContainer");
		redenContainer.setOutputMarkupPlaceholderTag(true);
		redenContainer.add(new EnumLabel<>("verzettenReden", afspraak.getVerzettenReden()));
		redenContainer.setVisible(isNieuweAfspraak && vanuitPlanning);
		add(redenContainer);

		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		WebMarkupContainer waarschuwing = new WebMarkupContainer("waarschuwing");
		waarschuwing.setVisible(isNieuweAfspraak && afspraakService.kortVoorVolgendeRonde(afspraak, isGeforceerd) && dossier.getTehuis() == null);
		add(waarschuwing);

		IndicatingAjaxLink<Void> wijzigMoment = new IndicatingAjaxLink<Void>("wijzigMoment")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				wijzigMoment(target);
			}
		};
		wijzigMoment.setVisible(isNieuweAfspraak);
		add(wijzigMoment);
	}

	@Override
	public void validate()
	{
		super.validate();
		MammaAfspraak nieuweAfspraak = getModelObject();
		if (nieuweAfspraak != null)
		{
			MammaVerzettenReden verzettenReden = nieuweAfspraak.getVerzettenReden();
			int aantalWerkdagenVerzettenVanaf = simplePreferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name());
			DateTime minimumAfspraakDatum = DateUtil.plusWerkdagen(dateSupplier.getDateTimeMidnight(), aantalWerkdagenVerzettenVanaf);
			if (MammaVerzettenReden.briefVerplicht(verzettenReden) && minimumAfspraakDatum.toDate().after(nieuweAfspraak.getVanaf()))
			{
				error("Voor deze afspraak wordt een bevestiging gestuurd omdat er geen overleg met client is geweest. De afspraak mag niet eerder zijn dan "
					+ aantalWerkdagenVerzettenVanaf + " werkdag(en) vanaf nu.");
			}
		}
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.put(ExtraOpslaanKey.MAMMA_BRIEF_AANMAKEN, briefAanmaken.getObject());
		opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BRIEF_TEGENHOUDEN, Boolean.TRUE.equals(uitnodigingsbriefTegenhouden.getObject()));
		return opslaanObjecten;
	}

	protected void wijzigMoment(AjaxRequestTarget target)
	{
	}
}
