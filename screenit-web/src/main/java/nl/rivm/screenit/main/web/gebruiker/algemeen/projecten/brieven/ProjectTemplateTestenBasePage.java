package nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.brieven;

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

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.dao.UitnodigingsDao;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestWrapper;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.DocumentTemplateTestenFieldsPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.projecten.ProjectBasePage;
import nl.rivm.screenit.model.BMHKLaboratorium;
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
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.MergeField;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsRonde;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.model.project.Project;
import nl.rivm.screenit.service.AsposeService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.collections.CollectionUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.aspose.words.Document;

public abstract class ProjectTemplateTestenBasePage extends ProjectBasePage
{

	private static final long serialVersionUID = 1L;

	private static final Logger LOG = LoggerFactory.getLogger(ProjectTemplateTestenBasePage.class);

	private enum TemplateBron
	{
		UPLOADED,
		AD_HOC
	}

	private IModel<ScreeningOrganisatie> selectedRegio = Model.of();

	private final TemplateBron bron;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private LogService logService;

	@SpringBean
	protected AsposeService asposeService;

	@SpringBean
	private UitnodigingsDao uitnodigingsDao;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	private IModel<List<FileUpload>> fileUploads = new ListModel<>();

	private DocumentTemplateTestenFieldsPanel fieldsContainer;

	private IModel<DocumentTemplateTestWrapper> wrapperModel = Model.of(new DocumentTemplateTestWrapper());

	public ProjectTemplateTestenBasePage(IModel<Project> model)
	{
		super(model);

		bron = null;

	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		MergeField.resetDefaultMergeFields();

		DocumentTemplateTestWrapper wrapper = wrapperModel.getObject();
		CervixUitnodiging uitnodiging = wrapper.getCervixUitnodiging();
		uitnodiging.setUitnodigingsId(uitnodigingsDao.getNextUitnodigingsId());
		List<ColoscopieCentrum> actieveIntakelocaties = instellingService.getActieveIntakelocaties();
		if (CollectionUtils.isNotEmpty(actieveIntakelocaties))
		{
			wrapper.cloneIntakeLocatie(actieveIntakelocaties.get(0));
		}

		ToegangLevel level = ScreenitSession.get().getToegangsLevel(Actie.AANPASSEN, Recht.GEBRUIKER_BEHEER_DOCUMENTENTEMPLATES);

		List<ScreeningOrganisatie> screeningOrganisatieLijst = getRegios();

		if (ToegangLevel.REGIO.equals(level))
		{
			screeningOrganisatieLijst.clear();
			ScreeningOrganisatie so = ScreenitSession.get().getScreeningOrganisatie();
			if (so != null)
			{
				screeningOrganisatieLijst.add(so);
			}
		}
		if (!screeningOrganisatieLijst.isEmpty())
		{
			selectedRegio = ModelUtil.sModel(screeningOrganisatieLijst.get(0));
		}

		Form<Void> form = new Form<>("regioForm");
		form.setMultiPart(true);
		add(form);

		ScreenitDropdown<ScreeningOrganisatie> screeningOrganisatieDropdown = new ScreenitDropdown<>("regio", selectedRegio,
			ModelUtil.listRModel(screeningOrganisatieLijst), new ChoiceRenderer<ScreeningOrganisatie>("naam"));
		screeningOrganisatieDropdown.setNullValid(false);
		screeningOrganisatieDropdown.setRequired(true);
		form.add(screeningOrganisatieDropdown);

		form.add(getPrintButton());

		RadioChoice<TemplateBron> bron = new RadioChoice<>("bron", new PropertyModel<TemplateBron>(this, "bron"), Arrays.asList(TemplateBron.values()),
			new EnumChoiceRenderer<TemplateBron>(this));
		bron.setPrefix("<label class=\"radio\">");
		bron.setSuffix("</label>");
		bron.setRequired(true);
		bron.setOutputMarkupId(true);
		FileUploadField uploadField = new FileUploadField("upload", fileUploads);
		uploadField.setOutputMarkupId(true);
		uploadField.setOutputMarkupPlaceholderTag(true);
		uploadField.setVisible(false);
		form.add(uploadField);
		form.add(bron);
		bron.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (ProjectTemplateTestenBasePage.this.bron == TemplateBron.AD_HOC)
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

			private static final long serialVersionUID = 1L;

			@Override
			protected List<MergeFieldTestType> getMergeTypes()
			{
				return ProjectTemplateTestenBasePage.this.getMergeTypes();
			}

		};
		form.add(fieldsContainer);
	}

	protected abstract List<ScreeningOrganisatie> getRegios();

	protected abstract void addAdditionalFormComponents(Form<Void> form);

	protected List<MergeFieldTestType> getMergeTypes()
	{
		List<MergeFieldTestType> values = new ArrayList<>(Arrays.asList(MergeFieldTestType.values()));
		values.remove(MergeFieldTestType.ZORGINSTELLING);
		values.remove(MergeFieldTestType.ZORGVERLENER);
		return values;
	}

	protected abstract List<Bevolkingsonderzoek> getBevolkingsonderzoeken();

	protected abstract Document proccesDocument(MailMergeContext context, File adHocBriefTemplate) throws Exception;

	private IndicatingAjaxSubmitLink getPrintButton()
	{
		IndicatingAjaxSubmitLink link = new IndicatingAjaxSubmitLink("printen")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onSubmit(AjaxRequestTarget target)
			{
				DocumentTemplateTestWrapper wrapper = wrapperModel.getObject();
				List<Bevolkingsonderzoek> bevolkingsonderzoeken = getBevolkingsonderzoeken();
				logService.logGebeurtenis(LogGebeurtenis.TESTEN_VAN_BRIEVEN, ScreenitSession.get().getLoggedInAccount(),
					bevolkingsonderzoeken.toArray(new Bevolkingsonderzoek[bevolkingsonderzoeken.size()]));

				ScreeningOrganisatie screeningOrganisatie = selectedRegio.getObject();

				MailMergeContext context = DocumentTemplateTestenFieldsPanel
					.createMailMergeContext(wrapper, screeningOrganisatie);

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
						briefTemplate = getBriefTemplateFile();
					}
					else if (bron != TemplateBron.UPLOADED && CollectionUtils.isNotEmpty(fileUploads.getObject()))
					{
						briefTemplate = fileUploads.getObject().get(0).writeToTempFile();
					}
					if (briefTemplate == null)
					{
						error("Er is geen template bestand opgegeven.");
						return;
					}

					Document mergedDocument = null;
					MammaBeoordeling laatsteBeoordelingMetUitslag = wrapper.getClient().getMammaDossier().getLaatsteBeoordelingMetUitslag();
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
						ColoscopieCentrum handmaktigeIntakeLocatie = location.getColoscopieCentrum();
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
						location.setColoscopieCentrum(handmaktigeIntakeLocatie);
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
		return link;
	}

	protected abstract File getBriefTemplateFile();

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(selectedRegio);
	}
}
