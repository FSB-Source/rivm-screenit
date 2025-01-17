package nl.rivm.screenit.main.web.gebruiker.screening.cervix.labformulier.controleren;

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

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.cervix.CervixHuisartsLocatie;
import nl.rivm.screenit.model.cervix.CervixLabformulier;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixLabformulierStatus;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.cervix.enums.signaleringen.CervixLabformulierSignalering;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.cervix.CervixLabformulierService;
import nl.rivm.screenit.service.cervix.CervixMailService;
import nl.rivm.screenit.service.cervix.CervixVervolgService;
import nl.rivm.screenit.service.cervix.enums.CervixVervolgTekst;
import nl.rivm.screenit.service.cervix.impl.CervixVervolg;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.EnumStringUtil;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormSubmitBehavior;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.AbstractChoice;
import org.apache.wicket.markup.html.form.CheckBoxMultipleChoice;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.wiquery.ui.datepicker.DatePicker;

import static nl.rivm.screenit.model.cervix.enums.signaleringen.CervixLabformulierSignalering.AFNAMEDATUM_NIET_OF_VERKEERD_OVERGENOMEN_IN_SCREENIT;

public abstract class CervixLabformulierBasePanel extends GenericPanel<CervixLabformulier>
{

	@SpringBean
	private CervixMailService cervixMailService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	protected ICurrentDateSupplier dateSupplier;

	@SpringBean
	private CervixLabformulierService labformulierService;

	@SpringBean
	private CervixVervolgService vervolgService;

	@SpringBean
	private LogService logService;

	private List<Long> labformulierenIds;

	private IModel<String> clientNaamModel;

	private WebMarkupContainer huisartsContainer;

	private WebMarkupContainer labformulierStatusContainer;

	private CervixLabformulierStatus vorigeStatus;

	private final Date vorigeDatumUitstrijkjeValue;

	public CervixLabformulierBasePanel(String id, List<Long> labformulierenIds, CervixLabformulier labformulier)
	{
		super(id, ModelUtil.ccModel(labformulier));
		this.labformulierenIds = labformulierenIds;
		vorigeDatumUitstrijkjeValue = labformulier.getDatumUitstrijkje();
		vorigeStatus = labformulier.getStatus();
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();

		saveLabformulier(true);

		CervixLabformulier labformulier = getModelObject();

		if (labformulier.getDigitaal())
		{
			add(new CervixDigitaalLabformulierPanel("labformulier"));
		}
		else if (labformulier.getKunstmatig())
		{
			add(new CervixKunstmatigLabformulierPanel("labformulier"));
		}
		else if (labformulier.getDatumGewist() != null)
		{
			add(new CervixVerwijderdLabformulierPanel("labformulier"));
		}
		else
		{
			add(new CervixLabformulierPanel("labformulier", getModelObject().getObjid()));
		}

		Form<CervixLabformulier> form = new Form<>("form", getModel());
		add(form);

		boolean magAanpassen = ScreenitSession.get().checkPermission(getRecht(), Actie.AANPASSEN);

		ComponentHelper.addTextField(form, "barcode", false, 12, Integer.class, !getMonsterIdEnabled() || !magAanpassen);

		WebMarkupContainer monsterstatusContainer = new WebMarkupContainer("monsterstatusContainer");
		form.add(monsterstatusContainer);
		EnumLabel<CervixUitstrijkjeStatus> monsterstatus = new EnumLabel<>("uitstrijkje.uitstrijkjeStatus");
		monsterstatus.setOutputMarkupId(true);
		monsterstatusContainer.add(monsterstatus);
		if (highlightMonsterstatus(labformulier))
		{
			monsterstatusContainer.add(new AttributeAppender("class", new Model<>("highlight"), " "));
		}

		Client client = getClient();
		clientNaamModel = new Model<>(NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(client));

		Label naam = new Label("clientNaam", clientNaamModel);
		naam.setOutputMarkupId(true);
		form.add(naam);
		Label bsn = new Label("uitstrijkje.uitnodiging.screeningRonde.dossier.client.persoon.bsn");
		bsn.setOutputMarkupId(true);
		form.add(bsn);
		Label geboortedatum = new Label("uitstrijkje.uitnodiging.screeningRonde.dossier.client.persoon.geboortedatum", DateUtil.getGeboortedatum(client));
		geboortedatum.setOutputMarkupId(true);
		form.add(geboortedatum);
		Label screeningOrganisatie = new Label("uitstrijkje.uitnodiging.screeningRonde.dossier.client.persoon.gbaAdres.gbaGemeente.screeningOrganisatie.naam");
		screeningOrganisatie.setOutputMarkupId(true);
		form.add(screeningOrganisatie);

		WebMarkupContainer datumUitstrijkjeContainer = new WebMarkupContainer("datumUitstrijkjeContainer");
		datumUitstrijkjeContainer.setVisible(getDatumUitstrijkjeVisible() && !labformulier.getKunstmatig());
		form.add(datumUitstrijkjeContainer);
		DatePicker<Date> datumUitstrijkje = ComponentHelper.newDatePicker("datumUitstrijkje", magAanpassen);
		datumUitstrijkjeContainer.add(datumUitstrijkje);

		WebMarkupContainer medischeGegevensContainer = new WebMarkupContainer("medischeGegevensContainer");
		medischeGegevensContainer.setVisible(getMedischeGegevensVisible() && !labformulier.getKunstmatig());
		form.add(medischeGegevensContainer);
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("klachtenGeen", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("klachtenContactbloedingen", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("klachtenKlachtenVanAbnormaleFluorZonderDuidelijkeOorzaak", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("klachtenIntermenstrueelBloedverlies", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("klachtenPostmenopauzaalBloedverlies", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("klachtenAndersNamelijk", magAanpassen));
		medischeGegevensContainer.add(textArea("klachtenAndersNamelijkTekst", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("menstruatieNormaal", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("menstruatieGeenMenstruatie", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("menstruatieMenopauze", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("menstruatiePostmenopauze", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newDatePicker("datumLaatsteMenstruatie", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("anticonceptieGeen", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("anticonceptiePil", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("anticonceptieIudKoper", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("anticonceptieIudMirena", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("anticonceptieAnders", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("gebruikHormonenJaVanwegeOvergangsklachten", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("gebruikHormonenJaVanwegeBorstkanker", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("gebruikHormonenJaVanwege", magAanpassen));
		medischeGegevensContainer.add(textArea("gebruikHormonenJaVanwegeTekst", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("gebruikHormonenGeen", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("aspectCervixNormaal", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("aspectCervixNietGezien", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("aspectCervixAbnormaalOfVerdachtePortio", magAanpassen));
		medischeGegevensContainer.add(textArea("aspectCervixAbnormaalOfVerdachtePortioTekst", magAanpassen));
		medischeGegevensContainer.add(ComponentHelper.newCheckBox("opmerkingen", magAanpassen));
		medischeGegevensContainer.add(textArea("opmerkingenTekst", magAanpassen));

		huisartsContainer = getHuisartsContainer(magAanpassen);
		form.add(huisartsContainer);

		form.add(new Label("scanDatum"));

		labformulierStatusContainer = new WebMarkupContainer("labformulierStatusContainer");
		labformulierStatusContainer.setVisible(getLabformulierStatusVisible());
		labformulierStatusContainer.setOutputMarkupId(true);
		form.add(labformulierStatusContainer);

		ScreenitDropdown<CervixLabformulierStatus> labformulierStatus = new ScreenitDropdown<>("status", mogelijkeStatussen(), new EnumChoiceRenderer<>());
		labformulierStatus.setEnabled(magAanpassen);
		labformulierStatusContainer.add(labformulierStatus);

		WebMarkupContainer labformulierSignaleringenContainer = new WebMarkupContainer("labformulierSignaleringenContainer");
		form.add(labformulierSignaleringenContainer);

		CheckBoxMultipleChoice<CervixLabformulierSignalering> labformulierSignaleringen = new CheckBoxMultipleChoice<>("signaleringen",
			CervixLabformulierSignalering.getMogelijkeSignaleringen(),
			new EnumChoiceRenderer<CervixLabformulierSignalering>()
			{
				@Override
				public Object getDisplayValue(CervixLabformulierSignalering object)
				{
					return object.getBeschrijving();
				}
			});
		labformulierSignaleringen.setOutputMarkupId(true);
		labformulierSignaleringen.setLabelPosition(AbstractChoice.LabelPosition.WRAP_AFTER);
		labformulierSignaleringenContainer.add(labformulierSignaleringen);

		TextField<String> overigeLabformulierSignalering = ComponentHelper.newTextField("overigeLabformulierSignalering", 255, false);
		labformulierSignaleringenContainer.add(overigeLabformulierSignalering);
		labformulierSignaleringenContainer.setVisible(getSignaleringenEnabled());

		Integer current = current();
		Link<Void> vorige = new Link<>("vorige")
		{
			@Override
			public void onClick()
			{
				setResponse(labformulierenIds, hibernateService.load(CervixLabformulier.class, labformulierenIds.get(current - 1)));
			}
		};
		vorige.setVisible(current > 0);
		add(vorige);

		add(new Label("nummer", current() + 1));
		add(new Label("van", labformulierenIds.size()));

		Link<Void> volgende = new Link<>("volgende")
		{
			@Override
			public void onClick()
			{
				setResponse(labformulierenIds, hibernateService.load(CervixLabformulier.class, labformulierenIds.get(current + 1)));
			}
		};
		volgende.setVisible(current < labformulierenIds.size() - 1);
		add(volgende);

		datumUitstrijkje.add(new AjaxFormComponentUpdatingBehavior("change")
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (!CervixLabformulierBasePanel.this.getModelObject().getDatumUitstrijkje().equals(vorigeDatumUitstrijkjeValue)
					&& !getModelObject().getSignaleringen().contains(AFNAMEDATUM_NIET_OF_VERKEERD_OVERGENOMEN_IN_SCREENIT))
				{
					getModelObject().getSignaleringen().add(AFNAMEDATUM_NIET_OF_VERKEERD_OVERGENOMEN_IN_SCREENIT);
				}
				target.add(labformulierSignaleringen);
			}
		});

		form.add(new AjaxFormSubmitBehavior("change")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				labformulierStatus.setChoices(mogelijkeStatussen());
				target.add(labformulierStatusContainer);

				target.add(naam);
				target.add(bsn);
				target.add(geboortedatum);
				target.add(screeningOrganisatie);

				saveLabformulier(false);

				clientNaamModel.setObject(NaamUtil.titelVoorlettersTussenvoegselEnAanspreekAchternaam(getClient()));
			}
		});
		logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_INGEZIEN, ScreenitSession.get().getLoggedInAccount(), client,
			getString("titel") + " - " + getString(EnumStringUtil.getPropertyString(getModelObject().getStatus())), Bevolkingsonderzoek.CERVIX);
	}

	protected boolean highlightMonsterstatus(CervixLabformulier labformulier)
	{
		return false;
	}

	private TextArea<String> textArea(String id, boolean magAanpassen)
	{
		TextArea<String> area = new TextArea<>(id);
		area.add(StringValidator.maximumLength(240));
		area.setEnabled(magAanpassen);
		return area;
	}

	private WebMarkupContainer getHuisartsContainer(boolean magAanpassen)
	{
		CervixLabformulier labformulier = getModelObject();
		WebMarkupContainer newHuisartsContainer;
		if (!magHuisartsWijzigen() || !magAanpassen || labformulier.getHuisartsLocatie() != null)
		{
			CompoundPropertyModel<CervixHuisartsLocatie> huisartsLocatieModel = new CompoundPropertyModel<>(
				new PropertyModel<>(CervixLabformulierBasePanel.this.getModel(), "huisartsLocatie"));

			newHuisartsContainer = new CervixHuisartsLocatiePanel("huisartsLocatieContainer", huisartsLocatieModel, magAanpassen && magHuisartsWijzigen())
			{
				private static final long serialVersionUID = 1L;

				@Override
				public void wijzigHuisartsLocatie(AjaxRequestTarget target)
				{
					replaceHuisartsLocatie(target, null);
				}
			};
		}
		else
		{
			newHuisartsContainer = new CervixZoekHuisartsLocatiePanel("huisartsLocatieContainer")
			{
				private static final long serialVersionUID = 1L;

				@Override
				protected boolean toonAdresVelden()
				{
					return magHuisartslocatieZoekenOpAdres();
				}

				@Override
				public void setHuisartsLocatie(AjaxRequestTarget target, CervixHuisartsLocatie huisartsLocatie)
				{
					replaceHuisartsLocatie(target, huisartsLocatie);
				}
			};
		}

		newHuisartsContainer.setOutputMarkupId(true);
		return newHuisartsContainer;
	}

	private void replaceHuisartsLocatie(AjaxRequestTarget target, CervixHuisartsLocatie huisartsLocatie)
	{
		CervixLabformulier labformulier = CervixLabformulierBasePanel.this.getModelObject();
		CervixVervolg vervolg = vervolgService.bepaalVervolg(labformulier.getUitstrijkje(), null);
		if (CervixLabformulierStatus.HUISARTS_ONBEKEND.equals(labformulier.getStatus()) && labformulier.getHuisartsLocatie() == null && huisartsLocatie != null
			&& vervolg.getVervolgTekst() != null
			&& CervixVervolgTekst.UITSTRIJKJE_HPV_POSITIEF_NAAR_CYTOLOGIE.getCssClass().equals(vervolg.getVervolgTekst().getCssClass()))
		{
			cervixMailService.sendHuisartsGekoppeldAanUitstrijkjeMail(labformulier.getUitstrijkje());
		}
		labformulier.setHuisartsLocatie(huisartsLocatie);
		saveLabformulier(false);
		WebMarkupContainer huisartsContainer = getHuisartsContainer(true);
		CervixLabformulierBasePanel.this.huisartsContainer.replaceWith(huisartsContainer);
		CervixLabformulierBasePanel.this.huisartsContainer = huisartsContainer;

		target.add(CervixLabformulierBasePanel.this.huisartsContainer);
		target.add(CervixLabformulierBasePanel.this.labformulierStatusContainer);
	}

	protected abstract boolean getDatumUitstrijkjeVisible();

	protected abstract boolean getMedischeGegevensVisible();

	protected abstract boolean getMonsterIdEnabled();

	protected abstract boolean getSignaleringenEnabled();

	protected abstract boolean magHuisartsWijzigen();

	protected boolean magHuisartslocatieZoekenOpAdres()
	{
		return false;
	}

	protected abstract CervixLabformulierStatus getVanStatus();

	protected abstract CervixLabformulierStatus getNaarStatus();

	protected abstract boolean getLabformulierStatusVisible();

	private Client getClient()
	{
		CervixUitstrijkje uitstrijkje = getModelObject().getUitstrijkje();
		if (uitstrijkje != null)
		{
			return uitstrijkje.getUitnodiging().getScreeningRonde().getDossier().getClient();
		}
		return null;
	}

	protected final void saveLabformulier(boolean onConstruction)
	{
		if (ScreenitSession.get().checkPermission(getRecht(), Actie.AANPASSEN))
		{
			CervixLabformulier labformulier = getModelObject();
			boolean gebruikerStatusNietAangepast = labformulier.getStatus().equals(vorigeStatus);
			try
			{

				boolean verzetStatus = getVanStatus() != null && getVanStatus() == labformulier.getStatus() && gebruikerStatusNietAangepast;
				if (verzetStatus)
				{
					labformulier.setStatus(getNaarStatus());
					labformulier.setStatusDatum(dateSupplier.getDate());
				}

				if (verzetStatus || !onConstruction)
				{
					labformulierService.valideerLabformulier(labformulier);

					labformulier = ModelProxyHelper.deproxy((CervixLabformulier) HibernateHelper.deproxy(labformulier));
					String diff = labformulierService.koppelEnBewaarLabformulier(labformulier);
					setModel(ModelUtil.ccModel(labformulier));

					logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_OPGESLAGEN, ScreenitSession.get().getLoggedInAccount(), getClient(),
						String.format("%s - %s %s %s", getString("titel"), getString(EnumStringUtil.getPropertyString(labformulier.getStatus())), getSignaleringen(), diff),
						Bevolkingsonderzoek.CERVIX);
					success(getString("labformulier.opgeslagen"));
				}
			}
			catch (IllegalStateException e)
			{
				error(getString(e.getMessage()));

				if (getNaarStatus() != null && labformulier.getStatus() == getNaarStatus() && gebruikerStatusNietAangepast)
				{
					labformulier.setStatus(getVanStatus());
					labformulier.setStatusDatum(dateSupplier.getDate());
					try
					{
						String diff = labformulierService.koppelEnBewaarLabformulier(labformulier);
						logService.logGebeurtenis(LogGebeurtenis.CERVIX_LABFORMULIER_OPGESLAGEN, ScreenitSession.get().getLoggedInAccount(), getClient(),
							String.format("%s - %s %s %s", getString("titel"), getString(EnumStringUtil.getPropertyString(labformulier.getStatus())), getSignaleringen(), diff),
							Bevolkingsonderzoek.CERVIX);
					}
					catch (Exception ex)
					{

					}
				}
			}
			vorigeStatus = labformulier.getStatus();
		}
	}

	private String getSignaleringen()
	{
		List<CervixLabformulierSignalering> signaleringen = getModelObject().getSignaleringen();
		if (CollectionUtils.isNotEmpty(signaleringen))
		{
			return " (signaleringen: " + StringUtils.join(signaleringen.stream().map(CervixLabformulierSignalering::getBeschrijving).collect(Collectors.toList()), ", ") + ")";
		}
		return "";
	}

	protected abstract void setResponse(List<Long> labformulierenIds, CervixLabformulier labformulier);

	protected abstract Recht getRecht();

	protected abstract List<CervixLabformulierStatus> permissiesVoorStatussen();

	private List<CervixLabformulierStatus> mogelijkeStatussen()
	{
		List<CervixLabformulierStatus> mogelijkeStatussen = permissiesVoorStatussen();
		if (!mogelijkeStatussen.contains(getModelObject().getStatus()))
		{
			mogelijkeStatussen.add(getModelObject().getStatus());
		}
		return mogelijkeStatussen;
	}

	private Integer current()
	{
		for (int i = 0; i < labformulierenIds.size(); i++)
		{
			if (labformulierenIds.get(i).equals(getModelObject().getId()))
			{
				return i;
			}
		}
		return null;
	}
}
