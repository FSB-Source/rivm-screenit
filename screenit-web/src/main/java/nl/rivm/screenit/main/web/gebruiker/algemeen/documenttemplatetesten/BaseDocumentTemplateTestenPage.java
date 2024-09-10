package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.BriefTypeChoiceRenderer;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.algemeen.AlgemeenPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.BriefDefinitie;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.colon.ColoscopieCentrum;
import nl.rivm.screenit.model.colon.Kamer;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.UploadDocumentService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.util.mamma.MammaScreeningRondeUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.event.Broadcast;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

import com.aspose.words.Document;

@Slf4j
public abstract class BaseDocumentTemplateTestenPage extends AlgemeenPage
{

	private enum TemplateBron
	{
		UPLOADED,
		AD_HOC
	}

	private IModel<ScreeningOrganisatie> selectedRegio;

	private final IModel<BriefType> selectedType;

	private final TemplateBron bron;

	protected final IModel<MergeField> mergeFieldModel = Model.of();

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private UploadDocumentService uploadDocumentService;

	@SpringBean
	protected AsposeService asposeService;

	@SpringBean
	private BaseBriefService briefService;

	@SpringBean
	private UitnodigingsDao uitnodigingsDao;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	private final IModel<List<FileUpload>> fileUploads = new ListModel<>();

	private final DocumentTemplateTestenFieldsPanel fieldsContainer;

	private final IModel<DocumentTemplateTestWrapper> wrapperModel = Model.of(new DocumentTemplateTestWrapper());

	public BaseDocumentTemplateTestenPage()
	{
		MergeField.resetDefaultMergeFields();

		DocumentTemplateTestWrapper wrapper = wrapperModel.getObject();
		CervixUitnodiging uitnodiging = wrapper.getCervixUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		List<ColoscopieCentrum> actieveIntakelocaties = instellingService.getActieveIntakelocaties();
		if (CollectionUtils.isNotEmpty(actieveIntakelocaties))
		{
			wrapper.cloneIntakeLocatie(actieveIntakelocaties.get(0));
		}

		ToegangLevel level = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, Recht.GEBRUIKER_BEHEER_DOCUMENTENTEMPLATES);

		List<ScreeningOrganisatie> screeningOrganisatieLijst = instellingService.getAllActiefScreeningOrganisaties();
		selectedType = Model.of();
		selectedRegio = ModelUtil.sModel(screeningOrganisatieLijst.get(0));

		if (ToegangLevel.REGIO.equals(level))
		{
			screeningOrganisatieLijst.clear();
			ScreeningOrganisatie so = ScreenitSession.get().getScreeningOrganisatie();
			if (so != null)
			{
				screeningOrganisatieLijst.add(so);
				selectedRegio = ModelUtil.sModel(so);
			}
		}

		bron = null;

		Form<Void> form = new Form<>("regioForm");
		form.setMultiPart(true);
		add(form);

		ScreenitDropdown<ScreeningOrganisatie> screeningOrganisatieDropdown = new ScreenitDropdown<>("regio", selectedRegio,
			ModelUtil.listRModel(screeningOrganisatieLijst), new ChoiceRenderer<>("naam"));
		screeningOrganisatieDropdown.setNullValid(false);
		screeningOrganisatieDropdown.setRequired(true);
		form.add(screeningOrganisatieDropdown);
		screeningOrganisatieDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				send(fieldsContainer, Broadcast.BREADTH, target);
			}

		});

		final ScreenitDropdown<BriefType> typeList = new ScreenitDropdown<>("brieven", new PropertyModel<>(this, "selectedType"),
			new IModel<List<BriefType>>()
			{

				@Override
				public List<BriefType> getObject()
				{
					return getVisibleBriefTypes();
				}

			}, new BriefTypeChoiceRenderer());
		typeList.setOutputMarkupId(true);
		form.add(typeList);
		form.add(getPrintButton());

		RadioChoice<TemplateBron> bron = new RadioChoice<>("bron", new PropertyModel<>(this, "bron"), Arrays.asList(TemplateBron.values()),
			new EnumChoiceRenderer<>(this));
		bron.setPrefix("<label class=\"radio\">");
		bron.setSuffix("</label>");
		bron.setRequired(true);
		bron.setOutputMarkupId(true);
		FileUploadField uploadField = new FileUploadField("upload", fileUploads);
		uploadField.setOutputMarkupId(true);
		uploadField.setOutputMarkupPlaceholderTag(true);
		uploadField.setVisible(false);
		uploadField.add(new FileValidator(FileType.WORD_NIEUW));
		form.add(uploadField);
		form.add(bron);
		bron.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (BaseDocumentTemplateTestenPage.this.bron == TemplateBron.AD_HOC)
				{
					uploadField.setVisible(true);
					target.add(uploadField);
				}
				else
				{
					uploadField.setVisible(false);
					target.add(uploadField);
				}
			}
		});

		addAdditionalFormComponents(form);

		fieldsContainer = new DocumentTemplateTestenFieldsPanel("fieldsContainer", wrapperModel)
		{
			@Override
			protected List<MergeFieldTestType> getMergeTypes()
			{
				return BaseDocumentTemplateTestenPage.this.getMergeTypes();
			}

		};
		form.add(fieldsContainer);
		addMergFieldSelector(form);
	}

	private void addMergFieldSelector(Form form)
	{
		var mergeFieldForm = new ScreenitForm<>("mergeFieldForm");
		form.add(mergeFieldForm);
		var mergeFieldDropdown = new ScreenitDropdown<>("mergeField", mergeFieldModel, List.of(MergeField.values()), new ChoiceRenderer<>("fieldName"));
		mergeFieldDropdown.setNullValid(true);
		mergeFieldDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				BaseDocumentTemplateTestenPage.this.mergeFieldChanged(target);
				target.add(form);
			}
		});
		mergeFieldForm.add(mergeFieldDropdown);
	}

	protected List<BriefType> zichtbareBriefTypesMetMergeField(List<BriefType> visibleBriefTypes, MergeField mergeField)
	{
		var briefTypes = new ArrayList<BriefType>();
		for (var briefType : visibleBriefTypes)
		{
			if (briefType.isActief())
			{
				var nieuwsteBriefDefinitie = briefService.getNieuwsteBriefDefinitie(briefType);
				if (nieuwsteBriefDefinitie != null)
				{
					var briefTemplate = uploadDocumentService.load(nieuwsteBriefDefinitie.getDocument());
					if (asposeService.heeftMergeField(briefTemplate, mergeField))
					{
						briefTypes.add(briefType);
					}
				}
			}
		}
		return briefTypes;
	}

	protected void mergeFieldChanged(AjaxRequestTarget target)
	{

	}

	protected abstract void addAdditionalFormComponents(Form<Void> form);

	protected abstract List<MergeFieldTestType> getMergeTypes();

	protected abstract List<BriefType> getVisibleBriefTypes();

	protected abstract List<Bevolkingsonderzoek> getBevolkingsonderzoeken();

	protected abstract Document proccesDocument(MailMergeContext context, File briefTemplate) throws Exception;

	private IndicatingAjaxSubmitLink getPrintButton()
	{
		return new IndicatingAjaxSubmitLink("printen")
		{

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				BriefType printType = getSelectedType();

				DocumentTemplateTestWrapper wrapper = wrapperModel.getObject();
				List<Bevolkingsonderzoek> bevolkingsonderzoeken = getBevolkingsonderzoeken();
				logService.logGebeurtenis(LogGebeurtenis.TESTEN_VAN_BRIEVEN, ScreenitSession.get().getLoggedInAccount(),
					bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[bevolkingsonderzoeken.size()]));

				ScreeningOrganisatie screeningOrganisatie = selectedRegio.getObject();

				MailMergeContext context = DocumentTemplateTestenFieldsPanel
					.createMailMergeContext(wrapper, screeningOrganisatie, printType);

				BMHKLaboratorium bmhkLaboratorium = wrapper.getBmhkLaboratorium();
				Client client = wrapper.getClient();
				Gemeente gbaGemeente = client.getPersoon().getGbaAdres().getGbaGemeente();
				ZASRetouradres zasRetouradres = bmhkLaboratorium.getRetouradressen().get(0);
				AfgeslotenMedewerkerOvereenkomst overeenkomst = wrapper.getOvereenkomst();
				CervixUitnodiging cervixUitnodiging = wrapper.getCervixUitnodiging();
				try
				{

					File briefTemplate = null;
					if (bron == TemplateBron.UPLOADED)
					{
						if (printType != null)
						{
							BriefDefinitie definitie = briefService.getNieuwsteBriefDefinitie(printType);
							briefTemplate = uploadDocumentService.load(definitie.getDocument());
						}
						else
						{
							error("Er is geen template gekozen.");
							return;
						}
					}
					else
					{
						if (CollectionUtils.isNotEmpty(fileUploads.getObject()))
						{
							briefTemplate = fileUploads.getObject().get(0).writeToTempFile();
						}
					}
					if (briefTemplate == null)
					{
						error("Er is geen template bestand opgegeven.");
						return;
					}

					Document mergedDocument = null;

					MammaBeoordeling laatsteBeoordelingMetUitslag = MammaScreeningRondeUtil.getLaatsteBeoordelingVanLaatsteOnderzoek(wrapper.getClient());
					Gebruiker handmatigeRadioloog1 = laatsteBeoordelingMetUitslag.getEersteLezing().getBeoordelaar().getMedewerker();
					Gebruiker handmatigeRadioloog2 = laatsteBeoordelingMetUitslag.getTweedeLezing().getBeoordelaar().getMedewerker();
					if (!wrapper.isFreeTextBKRADIOLOOG())
					{
						laatsteBeoordelingMetUitslag.getEersteLezing().getBeoordelaar().setMedewerker(wrapper.getRadioloog1());
						laatsteBeoordelingMetUitslag.getTweedeLezing().getBeoordelaar().setMedewerker(wrapper.getRadioloog2());
					}

					if (wrapper.isFromDBINTAKELOCATIE())
					{
						List<ColoscopieCentrum> intakeLocaties = instellingService.getActieveIntakelocatiesBinneRegio(screeningOrganisatie);
						Kamer location = wrapper.getIntakeAfspraak().getLocation();
						ColoscopieCentrum handmatigeIntakeLocatie = location.getColoscopieCentrum();
						for (ColoscopieCentrum intakeLocatie : intakeLocaties)
						{
							location.setColoscopieCentrum(intakeLocatie);
							Document document = proccesDocument(context, briefTemplate);

							mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);

						}
						if (intakeLocaties.isEmpty())
						{
							error("Geen intakelocaties die vallen in de regio " + screeningOrganisatie.getNaam());
						}
						location.setColoscopieCentrum(handmatigeIntakeLocatie);
					}
					else if (wrapper.isFromDBBMHKLAB())
					{
						List<BMHKLaboratorium> labs = instellingService.getActieveInstellingen(BMHKLaboratorium.class);

						for (BMHKLaboratorium lab : labs)
						{
							cervixUitnodiging.getMonster().setLaboratorium(lab);
							gbaGemeente.setBmhkLaboratorium(lab);
							context.setBmhkLaboratorium(lab);

							Document document = proccesDocument(context, briefTemplate);

							mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);

						}
						if (labs.isEmpty())
						{
							error("Geen BMHK laboratorium");
						}
						cervixUitnodiging.getMonster().setLaboratorium(bmhkLaboratorium);
						gbaGemeente.setBmhkLaboratorium(bmhkLaboratorium);
						context.setBmhkLaboratorium(bmhkLaboratorium);
					}
					else if (wrapper.isFromDBBKSTANDPLAATS())
					{
						List<MammaStandplaats> standplaatsen = standplaatsService.getActieveStandplaatsen(screeningOrganisatie);

						MammaStandplaatsRonde standplaatsRonde = context.getClient().getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak()
							.getStandplaatsPeriode().getStandplaatsRonde();
						MammaStandplaats handmatigeStandplaats = standplaatsRonde.getStandplaats();
						for (MammaStandplaats standplaats : standplaatsen)
						{
							standplaatsRonde.setStandplaats(standplaats);

							Document document = proccesDocument(context, briefTemplate);
							mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);
						}
						if (standplaatsen.isEmpty())
						{
							error("Geen standplaatsen die vallen in de regio " + screeningOrganisatie.getNaam());
						}
						standplaatsRonde.setStandplaats(handmatigeStandplaats);
					}
					else
					{
						Document document = proccesDocument(context, briefTemplate);
						mergedDocument = DocumentTemplateTestenFieldsPanel.addDocument(mergedDocument, document);
					}

					if (!wrapper.isFreeTextBKRADIOLOOG())
					{
						laatsteBeoordelingMetUitslag.getEersteLezing().getBeoordelaar().setMedewerker(handmatigeRadioloog1);
						laatsteBeoordelingMetUitslag.getTweedeLezing().getBeoordelaar().setMedewerker(handmatigeRadioloog2);
					}

					fieldsContainer.createAndShowPDF(target, mergedDocument);
				}
				catch (Exception e)
				{
					LOG.error("Error converting doc to pdf", e);
					error("Er is iets misgegaan met het genereren van de pdf.");
				}
				finally
				{
					gbaGemeente.setScreeningOrganisatie(null);
					zasRetouradres.setRegio(null);
					overeenkomst.setScreeningOrganisatie(null);
				}
			}
		};
	}

	public BriefType getSelectedType()
	{
		return ModelUtil.nullSafeGet(selectedType);
	}

	public void setSelectedType(BriefType selectedType)
	{
		this.selectedType.setObject(selectedType);
	}

	public ScreeningOrganisatie getSelectedRegio()
	{
		return ModelUtil.nullSafeGet(selectedRegio);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(selectedType);
		ModelUtil.nullSafeDetach(selectedRegio);
		ModelUtil.nullSafeDetach(wrapperModel);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<GebruikerMenuItem>();
		contextMenuItems.add(new GebruikerMenuItem("label.documententemplates.overige", DocumentTemplateTestenPage.class));
		contextMenuItems.add(new GebruikerMenuItem("label.documententemplates.bezwaar", BezwaarDocumentenTemplatesPage.class));
		return contextMenuItems;
	}
}
