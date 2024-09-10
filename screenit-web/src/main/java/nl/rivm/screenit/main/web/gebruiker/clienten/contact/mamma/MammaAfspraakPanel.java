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

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Map;

import nl.rivm.screenit.PreferenceKey;
import nl.rivm.screenit.main.service.mamma.MammaAfspraakService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.validator.EmailAddressValidator;
import nl.rivm.screenit.main.web.component.validator.ScreenitTelefoonnummerValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.ClientContactPanel;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.BevestigingsType;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.SmsStatus;
import nl.rivm.screenit.model.mamma.MammaAfspraak;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.preferencemodule.service.SimplePreferenceService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

public class MammaAfspraakPanel extends AbstractClientContactActiePanel<MammaAfspraak>
{
	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private SimplePreferenceService preferenceService;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private MammaAfspraakService afspraakService;

	private boolean isNieuweAfspraak;

	private BevestigingsType gekozenBevestiging = BevestigingsType.MAIL;

	private SmsStatus gekozenSmsStatus = SmsStatus.TE_VERSTUREN;

	private WebMarkupContainer mailveld;

	private WebMarkupContainer mobielNummerContainer;

	private IModel<String> emailNieuwModel;

	private IModel<String> mobielNummerNieuwModel;

	public MammaAfspraakPanel(String id, MammaAfspraak afspraak, boolean isNieuweAfspraak)
	{
		super(id, ModelUtil.ccModel(afspraak));
		this.isNieuweAfspraak = isNieuweAfspraak;
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

		boolean vanuitPlanning = getPage().getMetaData(ClientContactPanel.CREATE_CONTEXT_KEY).bkVanuitPlanning;

		add(maakAfspraakBevestigingsOpties(afspraak));
		mailveld = maakEmailInvoer();
		add(mailveld);
		maakSmsBevestigingVelden(afspraak);

		WebMarkupContainer redenContainer = new WebMarkupContainer("redenContainer");
		redenContainer.setOutputMarkupPlaceholderTag(true);
		redenContainer.add(new EnumLabel<>("verzettenReden", afspraak.getVerzettenReden()));
		redenContainer.setVisible(isNieuweAfspraak && vanuitPlanning);
		add(redenContainer);

		MammaDossier dossier = afspraak.getUitnodiging().getScreeningRonde().getDossier();
		WebMarkupContainer waarschuwing = new WebMarkupContainer("waarschuwing");
		waarschuwing.setVisible(isNieuweAfspraak && afspraakService.kortVoorVolgendeRonde(afspraak) && dossier.getTehuis() == null);
		add(waarschuwing);

		IndicatingAjaxLink<Void> wijzigMoment = new IndicatingAjaxLink<>("wijzigMoment")
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

	private WebMarkupContainer maakAfspraakBevestigingsOpties(MammaAfspraak afspraak)
	{
		var magBriefBevestiging = baseAfspraakService.briefKanNogVerzondenWorden(afspraak.getVanaf());

		var mogelijkeBevestigingLijst = new ArrayList<>(Arrays.asList(BevestigingsType.values()));
		if (!magBriefBevestiging)
		{
			mogelijkeBevestigingLijst.remove(BevestigingsType.BRIEF);
		}

		RadioChoice<BevestigingsType> afspraakBevestigingRadio = new RadioChoice<>("afspraakBevestiging", new PropertyModel<>(this, "gekozenBevestiging"),
			mogelijkeBevestigingLijst, new EnumChoiceRenderer<>(this));
		afspraakBevestigingRadio.setRequired(true);
		afspraakBevestigingRadio.setVisible(isNieuweAfspraak);
		afspraakBevestigingRadio.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				setMailveldZichtbaar(target);
			}
		});

		return afspraakBevestigingRadio;
	}

	private void maakSmsBevestigingVelden(MammaAfspraak afspraak)
	{
		var magSmsBevestiging = baseAfspraakService.smsKanNogVerzondenWorden(DateUtil.toLocalDateTime(afspraak.getVanaf()));
		if (!magSmsBevestiging)
		{
			gekozenSmsStatus = SmsStatus.GEEN;
		}

		add(maakSmsBevestigingKeuzeRadio(magSmsBevestiging));
		add(maakMobielNummerInvoerveld());
	}

	private RadioChoice<SmsStatus> maakSmsBevestigingKeuzeRadio(boolean magSmsBevestiging)
	{
		RadioChoice<SmsStatus> smsBevestigingsKeuzeRadio = new RadioChoice<>("smsStatus", new PropertyModel<>(this, "gekozenSmsStatus"),
			SmsStatus.handmatigTeKiezenStatussen(), new EnumChoiceRenderer<>(this));
		smsBevestigingsKeuzeRadio.setRequired(true);
		smsBevestigingsKeuzeRadio.setVisible(magSmsBevestiging && isNieuweAfspraak);
		smsBevestigingsKeuzeRadio.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				setMobielNummerInvoerveldZichtbaar(target);
			}
		});
		return smsBevestigingsKeuzeRadio;
	}

	private WebMarkupContainer maakMobielNummerInvoerveld()
	{
		mobielNummerContainer = new WebMarkupContainer("mobielNummerContainer");
		mobielNummerContainer.setVisible(SmsStatus.TE_VERSTUREN == gekozenSmsStatus && isNieuweAfspraak);
		mobielNummerContainer.setOutputMarkupPlaceholderTag(true);

		var persoon = getModel().getObject().getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon();
		var mobielNummerHuidig = persoon.getTelefoonnummer1();
		mobielNummerNieuwModel = Model.of(mobielNummerHuidig);

		var mobielNummerInvoerveld = new TextField<>("mobielNummer", mobielNummerNieuwModel);
		mobielNummerInvoerveld.setOutputMarkupPlaceholderTag(true);
		mobielNummerInvoerveld.add(ScreenitTelefoonnummerValidator.mobielNederlandsNummer());
		mobielNummerInvoerveld.add(StringValidator.maximumLength(GbaPersoon.MAX_PHONE_LENGTH));
		mobielNummerInvoerveld.setRequired(true);

		mobielNummerContainer.add(mobielNummerInvoerveld);
		return mobielNummerContainer;
	}

	@Override
	public void validate()
	{
		super.validate();
		MammaAfspraak nieuweAfspraak = getModelObject();
		if (nieuweAfspraak != null)
		{
			MammaVerzettenReden verzettenReden = nieuweAfspraak.getVerzettenReden();
			int aantalWerkdagenVerzettenVanaf = preferenceService.getInteger(PreferenceKey.MAMMA_AFSPRAAK_VERZETTEN_ZONDER_CLIENT_CONTACT_VANAF_AANTAL_WERKDAGEN.name());
			LocalDate minimumAfspraakDatum = DateUtil.plusWerkdagen(dateSupplier.getLocalDate(), aantalWerkdagenVerzettenVanaf);
			if (MammaVerzettenReden.briefVerplicht(verzettenReden) && DateUtil.toUtilDate(minimumAfspraakDatum).after(nieuweAfspraak.getVanaf()))
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
		opslaanObjecten.put(ExtraOpslaanKey.BEVESTIGINGS_TYPE, gekozenBevestiging);
		opslaanObjecten.put(ExtraOpslaanKey.SMS_STATUS, gekozenSmsStatus);
		if (BevestigingsType.MAIL == gekozenBevestiging)
		{
			opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_BEVESTIGING_MAIL_ADRES, emailNieuwModel.getObject());
		}
		if (SmsStatus.TE_VERSTUREN == gekozenSmsStatus)
		{
			opslaanObjecten.put(ExtraOpslaanKey.AFSPRAAK_HERINNERING_TELEFOONNUMMER, mobielNummerNieuwModel.getObject());
		}
		return opslaanObjecten;
	}

	protected void wijzigMoment(AjaxRequestTarget target)
	{
	}

	private void setMailveldZichtbaar(AjaxRequestTarget target)
	{
		mailveld.setVisible(BevestigingsType.MAIL == gekozenBevestiging);
		target.add(mailveld);
	}

	private void setMobielNummerInvoerveldZichtbaar(AjaxRequestTarget target)
	{
		mobielNummerContainer.setVisible(SmsStatus.TE_VERSTUREN == gekozenSmsStatus);
		target.add(mobielNummerContainer);
	}

	private WebMarkupContainer maakEmailInvoer()
	{
		var persoon = getModel().getObject().getUitnodiging().getScreeningRonde().getDossier().getClient().getPersoon();
		var emailHuidig = persoon.getEmailadres();
		emailNieuwModel = Model.of(emailHuidig);

		var textField = new TextField<>("emailadres", emailNieuwModel);
		textField.setEnabled(true);
		textField.setOutputMarkupPlaceholderTag(true);
		setEmailValidaties(textField);

		var emailContainer = new WebMarkupContainer("emailadresContainer");
		emailContainer.add(textField);
		emailContainer.setOutputMarkupPlaceholderTag(true);
		emailContainer.setVisible(isNieuweAfspraak);

		return emailContainer;
	}

	private void setEmailValidaties(TextField<String> emailVeld)
	{
		emailVeld.add(StringValidator.maximumLength(GbaPersoon.MAX_EMAIL_LENGTH));
		emailVeld.add(EmailAddressValidator.getInstance());
		emailVeld.setRequired(true);
	}
}
