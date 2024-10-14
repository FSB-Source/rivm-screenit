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

import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.BooleanFragment;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.ClientFieldsFragment;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.IntakeLocatieFieldsFragment;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.MergeFieldsFragment;
import nl.rivm.screenit.model.Brief;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.algemeen.BezwaarBrief;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.colon.ColonBrief;
import nl.rivm.screenit.model.enums.BriefType;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.mamma.MammaBrief;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.hibernate.spring.util.ApplicationContextProvider;

import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.behavior.AttributeAppender;
import org.apache.wicket.event.Broadcast;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.head.OnDomReadyHeaderItem;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.EnumLabel;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.FormComponent;
import org.apache.wicket.markup.html.form.IChoiceRenderer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.Fragment;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;

import com.aspose.words.Document;
import com.aspose.words.HeaderFooterType;
import com.aspose.words.ImportFormatMode;

public abstract class DocumentTemplateTestenFieldsPanel extends GenericPanel<DocumentTemplateTestWrapper>
{
	@SpringBean
	private BaseBriefService briefService;

	private final BootstrapDialog printDialog;

	protected DocumentTemplateTestenFieldsPanel(String id, IModel<DocumentTemplateTestWrapper> wrapperModel)
	{
		super(id, wrapperModel);
		this.printDialog = new BootstrapDialog("printDialog");
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(printDialog);

		var container = new WebMarkupContainer("container");
		container.add(getMergeFieldTestTypeListView(container.getMarkupId()));
		add(container);
	}

	public static FormComponent<String> getTextAreaWithStringValidator(String wicketId, int maxStringLength)
	{
		return new TextArea<String>(wicketId)
			.add(StringValidator.maximumLength(maxStringLength));
	}

	public static FormComponent<String> getTextAreaWithStringValidator(String wicketId, int maxStringLength, IModel<String> model)
	{
		return new TextArea<>(wicketId, model)
			.add(StringValidator.maximumLength(maxStringLength));
	}

	public static <T> ScreenitDropdown<T> getScreenitDropdown(String wicketId, List<T> choices, ChoiceRenderer<T> choiceRenderer, boolean nullValid)
	{
		return new ScreenitDropdown<>(wicketId, choices, choiceRenderer)
			.setNullValid(nullValid);
	}

	public static <T> ScreenitDropdown<T> getScreenitDropdown(String wicketId, IModel<T> model, List<? extends T> choices, boolean nullValid)
	{
		return new ScreenitDropdown<>(wicketId, model, choices)
			.setNullValid(nullValid);
	}

	public static <T> MarkupContainer getScreenitDropdown(String wicketId, IModel<T> model, List<? extends T> choices, IChoiceRenderer<? super T> renderer, boolean nullValid)
	{
		return new WebMarkupContainer("formContainer")
			.add(new ScreenitDropdown<>(wicketId, model, choices, renderer)
				.setNullValid(nullValid)
				.setOutputMarkupId(true));
	}

	public static <T> MarkupContainer getScreenitDropdown(String wicketId, IModel<T> model, IModel<List<T>> choices, IChoiceRenderer<? super T> renderer, boolean nullValid)
	{
		return new WebMarkupContainer("formContainer")
			.add(new ScreenitDropdown<>(wicketId, model, choices, renderer)
				.setNullValid(nullValid)
				.setOutputMarkupId(true));
	}

	public static <T> TextField<T> getTextField(String wicketId, IModel<T> model, Class<T> clss)
	{
		return new TextField<>(wicketId, model, clss);
	}

	private ListView<MergeFieldTestType> getMergeFieldTestTypeListView(String containerMarkupId)
	{
		return new ListView<>("mergeTypes", getMergeTypes())
		{
			@Override
			protected void populateItem(ListItem<MergeFieldTestType> item)
			{
				var mergeFieldTestType = item.getModelObject();
				var content = getContent(mergeFieldTestType);
				item.add(content);
				item.add(getCollapseLink(mergeFieldTestType, content.getMarkupId()));
			}

			private WebMarkupContainer getCollapseLink(MergeFieldTestType mergeFieldTestType, String contentMarkupId)
			{
				var collapseLink = new WebMarkupContainer("collapseLink");
				collapseLink.add(new AttributeAppender("data-parent", new Model<>("#" + containerMarkupId)));
				collapseLink.add(new AttributeAppender("href", new Model<>("#" + contentMarkupId)));
				collapseLink.add(new EnumLabel<>("mergeTypeLabel", mergeFieldTestType));
				return collapseLink;
			}

			private WebMarkupContainer getContent(MergeFieldTestType mergeFieldTestType)
			{
				var content = new WebMarkupContainer("content");
				content.add(getFromDBLabel(mergeFieldTestType));
				content.add(getCheckBox(mergeFieldTestType));
				content.add(getField(mergeFieldTestType));
				content.add(getExtraField(mergeFieldTestType));
				return content;
			}

			private Label getFromDBLabel(MergeFieldTestType mergeFieldTestType)
			{
				return new Label("fromDBLabel", mergeFieldTestType.isFromDB()
					? "Gegevens uit database?:"
					: "Vrije tekst invullen?:");
			}

			private Component getCheckBox(MergeFieldTestType mergeFieldTestType)
			{
				if (!mergeFieldTestType.isFromDB() && mergeFieldTestType.isFreeText())
				{
					return new AjaxCheckBox("fromDB", new PropertyModel<>(DocumentTemplateTestenFieldsPanel.this.getModel(), "freeText" + mergeFieldTestType.name()))
					{
						@Override
						protected void onUpdate(AjaxRequestTarget target)
						{
							send(DocumentTemplateTestenFieldsPanel.this, Broadcast.BREADTH, target);
						}
					};
				}
				return new AjaxCheckBox("fromDB", new PropertyModel<>(DocumentTemplateTestenFieldsPanel.this.getModel(), "fromDB" + mergeFieldTestType.name()))
				{
					@Override
					protected void onUpdate(AjaxRequestTarget target)
					{
						send(DocumentTemplateTestenFieldsPanel.this, Broadcast.BREADTH, target);
					}
				}.setVisible(mergeFieldTestType.isFromDB());
			}

			private Fragment getField(MergeFieldTestType mergeFieldTestType)
			{
				if (mergeFieldTestType == MergeFieldTestType.INTAKELOCATIE)
				{
					return new IntakeLocatieFieldsFragment("fields", DocumentTemplateTestenFieldsPanel.this, DocumentTemplateTestenFieldsPanel.this.getModel());
				}
				else if (mergeFieldTestType == MergeFieldTestType.CLIENT)
				{
					return new ClientFieldsFragment("fields", DocumentTemplateTestenFieldsPanel.this, DocumentTemplateTestenFieldsPanel.this.getModel());
				}
				return new MergeFieldsFragment("fields", DocumentTemplateTestenFieldsPanel.this, mergeFieldTestType, DocumentTemplateTestenFieldsPanel.this.getModel());
			}

			private Component getExtraField(MergeFieldTestType mergeFieldTestType)
			{
				return mergeFieldTestType == MergeFieldTestType.BMHKLAB
					? new BooleanFragment("extraField", DocumentTemplateTestenFieldsPanel.this,
					new PropertyModel<>(DocumentTemplateTestenFieldsPanel.this.getModel(), "microbioloog"))
					: new EmptyPanel("extraField").setVisible(false);
			}

			@Override
			public void renderHead(IHeaderResponse response)
			{
				super.renderHead(response);
				response.render(OnDomReadyHeaderItem.forScript("toggleChevron();"));
			}
		};
	}

	public static Document addDocument(Document mergedDocument, Document document)
	{
		return mergedDocument == null
			? document
			: getAppendedMergedDocument(mergedDocument, document);
	}

	private static Document getAppendedMergedDocument(Document mergedDocument, Document document)
	{
		if (headerLacking(document))
		{
			document.getFirstSection()
				.getHeadersFooters()
				.linkToPrevious(false);
		}
		mergedDocument.appendDocument(document, ImportFormatMode.USE_DESTINATION_STYLES);
		return mergedDocument;
	}

	private static boolean headerLacking(Document document)
	{
		return Arrays.stream(document.getLastSection().getHeadersFooters().toArray())
			.noneMatch(nodes -> HeaderFooterType.HEADER_FIRST == nodes.getHeaderFooterType()
				|| HeaderFooterType.HEADER_EVEN == nodes.getHeaderFooterType()
				|| HeaderFooterType.HEADER_PRIMARY == nodes.getHeaderFooterType());
	}

	public void createAndShowPDF(AjaxRequestTarget target, Document mergedDocument) throws Exception
	{
		printDialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, briefService.genereerPdf(mergedDocument, "test_template_brieven", true)));
	}

	public static MailMergeContext createMailMergeContext(DocumentTemplateTestWrapper wrapper, ScreeningOrganisatie screeningOrganisatie)
	{
		var context = maakBasicContext(wrapper, screeningOrganisatie);
		var brief = maakCervixBrief(wrapper);
		context.setBrief(brief);
		return context;
	}

	public static MailMergeContext createMailMergeContext(DocumentTemplateTestWrapper wrapper, ScreeningOrganisatie screeningOrganisatie, BriefType printType,
		boolean zonderHandtekening)
	{
		var context = maakBasicContext(wrapper, screeningOrganisatie);

		Brief brief;
		if (printType != null)
		{
			if (BriefType.CLIENT_BEZWAAR_BRIEVEN.contains(printType))
			{
				brief = new BezwaarBrief();
				((BezwaarBrief) brief).setVragenOmHandtekening(zonderHandtekening);
			}
			else
			{
				switch (printType.getOnderzoeken()[0])
				{
				case COLON:
					brief = new ColonBrief();
					break;
				case CERVIX:
					brief = maakCervixBrief(wrapper);
					break;
				case MAMMA:
					brief = new MammaBrief();
					break;
				default:
					throw new IllegalStateException("Brief heeft altijd een BVO type");
				}
			}
			brief.setBriefType(printType);
			context.setBrief(brief);
		}

		return context;
	}

	private static CervixBrief maakCervixBrief(DocumentTemplateTestWrapper wrapper)
	{
		var cervixBrief = new CervixBrief();
		var cervixUitnodiging = wrapper.getCervixUitnodiging();

		cervixBrief.setMonster(cervixUitnodiging.getMonster());
		var cervixUitstrijkje = (CervixUitstrijkje) cervixUitnodiging.getMonster();
		if (wrapper.isFromDBBMHKLAB()
			&& wrapper.isMicrobioloog())
		{
			cervixUitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.ONTVANGEN);
		}
		else
		{
			cervixUitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.BEOORDEELD_DOOR_CYTOLOGIE);
		}

		return cervixBrief;
	}

	private static MailMergeContext maakBasicContext(DocumentTemplateTestWrapper wrapper, ScreeningOrganisatie screeningOrganisatie)
	{
		var bmhkLaboratorium = wrapper.getBmhkLaboratorium();
		var zasRetouradres = bmhkLaboratorium.getRetouradressen().get(0);
		var cervixUitnodiging = wrapper.getCervixUitnodiging();
		var overeenkomst = wrapper.getOvereenkomst();
		var client = wrapper.getClient();
		var gbaGemeente = client.getPersoon().getGbaAdres().getGbaGemeente();

		gbaGemeente.setScreeningOrganisatie(screeningOrganisatie);
		zasRetouradres.setRegio(screeningOrganisatie);
		overeenkomst.setScreeningOrganisatie(screeningOrganisatie);

		var context = new MailMergeContext();
		context.setUseTestValue(true);
		context.setClient(client);
		context.setCervixUitnodiging(cervixUitnodiging);
		context.setOvereenkomst(overeenkomst);
		context.putValue(MailMergeContext.CONTEXT_SCREENING_ORGANISATIE, screeningOrganisatie);
		context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, ApplicationContextProvider.getApplicationContext().getBean(ClientService.class).bepaalCe(client));
		context.setIntakeAfspraak(wrapper.getIntakeAfspraak());
		context.setBmhkLaboratorium(bmhkLaboratorium);
		return context;
	}

	protected abstract List<MergeFieldTestType> getMergeTypes();

}
