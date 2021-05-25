package nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.BooleanFragment;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.ClientFieldsFragment;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.IntakeLocatieFieldsFragment;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.fragments.MergeFieldsFragment;
import nl.rivm.screenit.model.BMHKLaboratorium;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.MailMergeContext;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.ZASRetouradres;
import nl.rivm.screenit.model.cervix.CervixBrief;
import nl.rivm.screenit.model.cervix.CervixUitnodiging;
import nl.rivm.screenit.model.cervix.CervixUitstrijkje;
import nl.rivm.screenit.model.cervix.enums.CervixUitstrijkjeStatus;
import nl.rivm.screenit.model.enums.MergeFieldTestType;
import nl.rivm.screenit.model.overeenkomsten.AfgeslotenMedewerkerOvereenkomst;
import nl.rivm.screenit.service.BaseBriefService;
import nl.rivm.screenit.service.ClientService;
import nl.topicuszorg.spring.injection.SpringBeanProvider;

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

	public DocumentTemplateTestenFieldsPanel(final String id,
		final IModel<DocumentTemplateTestWrapper> wrapperModel)
	{
		super(id, wrapperModel);
		this.printDialog = new BootstrapDialog("printDialog");
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		add(printDialog);

		final WebMarkupContainer container = new WebMarkupContainer("container");
		container.add(getMergeFieldTestTypeListView(container.getMarkupId()));
		add(container);
	}

	public static FormComponent<String> getTextAreaWithStringValidator(final String wicketId,
		final int maxStringLength)
	{
		return new TextArea<String>(wicketId)
			.add(StringValidator.maximumLength(maxStringLength));
	}

	public static <T> ScreenitDropdown<T> getScreenitDropdown(final String wicketId,
		final List<T> choices,
		final ChoiceRenderer<T> choiceRenderer,
		final boolean nullValid)
	{
		return new ScreenitDropdown<>(wicketId, choices, choiceRenderer)
			.setNullValid(nullValid);
	}

	public static <T> ScreenitDropdown<T> getScreenitDropdown(final String wicketId,
		final IModel<T> model,
		final List<? extends T> choices,
		final boolean nullValid)
	{
		return new ScreenitDropdown<>(wicketId, model, choices)
			.setNullValid(nullValid);
	}

	public static <T> MarkupContainer getScreenitDropdown(final String wicketId,
		final IModel<T> model,
		final List<? extends T> choices,
		final IChoiceRenderer<? super T> renderer,
		final boolean nullValid)
	{
		return new WebMarkupContainer("formContainer")
			.add(new ScreenitDropdown<>(wicketId, model, choices, renderer)
				.setNullValid(nullValid)
				.setOutputMarkupId(true));
	}

	public static <T> MarkupContainer getScreenitDropdown(final String wicketId,
		final IModel<T> model,
		final IModel<List<T>> choices,
		final IChoiceRenderer<? super T> renderer,
		final boolean nullValid)
	{
		return new WebMarkupContainer("formContainer")
			.add(new ScreenitDropdown<>(wicketId, model, choices, renderer)
				.setNullValid(nullValid)
				.setOutputMarkupId(true));
	}

	public static <T> TextField<T> getTextField(final String wicketId,
		final Class<T> clss)
	{
		return new TextField<>(wicketId, clss);
	}

	public static <T> TextField<T> getTextField(final String wicketId,
		final IModel<T> model,
		final Class<T> clss)
	{
		return new TextField<>(wicketId, model, clss);
	}

	private ListView<MergeFieldTestType> getMergeFieldTestTypeListView(final String containerMarkupId)
	{
		return new ListView<MergeFieldTestType>("mergeTypes", getMergeTypes())
		{
			@Override
			protected void populateItem(final ListItem<MergeFieldTestType> item)
			{
				MergeFieldTestType mergeFieldTestType = item.getModelObject();
				WebMarkupContainer content = getContent(mergeFieldTestType);
				item.add(content);
				item.add(getCollapseLink(mergeFieldTestType, content.getMarkupId()));
			}

			private WebMarkupContainer getCollapseLink(final MergeFieldTestType mergeFieldTestType,
				final String contentMarkupId)
			{
				WebMarkupContainer collapseLink = new WebMarkupContainer("collapseLink");
				collapseLink.add(new AttributeAppender("data-parent", new Model<>("#" + containerMarkupId)));
				collapseLink.add(new AttributeAppender("href", new Model<>("#" + contentMarkupId)));
				collapseLink.add(new EnumLabel<>("mergeTypeLabel", mergeFieldTestType));
				return collapseLink;
			}

			private WebMarkupContainer getContent(final MergeFieldTestType mergeFieldTestType)
			{
				WebMarkupContainer content = new WebMarkupContainer("content");
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

			private Component getCheckBox(final MergeFieldTestType mergeFieldTestType)
			{
				if (!mergeFieldTestType.isFromDB()
					&& mergeFieldTestType.isFreeText())
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

			private Fragment getField(final MergeFieldTestType mergeFieldTestType)
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

			private Component getExtraField(final MergeFieldTestType mergeFieldTestType)
			{
				return mergeFieldTestType == MergeFieldTestType.BMHKLAB
					? new BooleanFragment("extraField", DocumentTemplateTestenFieldsPanel.this,
						new PropertyModel<>(DocumentTemplateTestenFieldsPanel.this.getModel(), "microbioloog"))
					: new EmptyPanel("extraField").setVisible(false);
			}

			@Override
			public void renderHead(final IHeaderResponse response)
			{
				super.renderHead(response);
				response.render(OnDomReadyHeaderItem.forScript("toggleChevron();"));
			};
		};
	}

	public static Document addDocument(final Document mergedDocument,
		final Document document)
	{
		return mergedDocument == null
			? document
			: getAppendedMergedDocument(mergedDocument, document);
	}

	private static Document getAppendedMergedDocument(final Document mergedDocument,
		final Document document)
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

	private static boolean headerLacking(final Document document)
	{
		return Arrays.stream(document.getLastSection().getHeadersFooters().toArray())
			.noneMatch(nodes -> HeaderFooterType.HEADER_FIRST == nodes.getHeaderFooterType()
				|| HeaderFooterType.HEADER_EVEN == nodes.getHeaderFooterType()
				|| HeaderFooterType.HEADER_PRIMARY == nodes.getHeaderFooterType());
	}

	public void createAndShowPDF(final AjaxRequestTarget target,
		final Document mergedDocument) throws IOException, Exception, FileNotFoundException
	{
		printDialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, briefService.genereerPdf(mergedDocument, "test_template_brieven", true)));
	}

	public static MailMergeContext createMailMergeContext(final DocumentTemplateTestWrapper wrapper,
		final ScreeningOrganisatie screeningOrganisatie)
	{
		BMHKLaboratorium bmhkLaboratorium = wrapper.getBmhkLaboratorium();
		ZASRetouradres zasRetouradres = bmhkLaboratorium.getRetouradressen().get(0);
		CervixUitnodiging cervixUitnodiging = wrapper.getCervixUitnodiging();
		AfgeslotenMedewerkerOvereenkomst overeenkomst = wrapper.getOvereenkomst();
		Client client = wrapper.getClient();
		Gemeente gbaGemeente = client.getPersoon().getGbaAdres().getGbaGemeente();

		gbaGemeente.setScreeningOrganisatie(screeningOrganisatie);
		zasRetouradres.setRegio(screeningOrganisatie);
		overeenkomst.setScreeningOrganisatie(screeningOrganisatie);

		MailMergeContext context = new MailMergeContext();
		context.setUseTestValue(Boolean.TRUE);
		context.setClient(client);
		context.setCervixUitnodiging(cervixUitnodiging);
		context.setOvereenkomst(overeenkomst);
		context.putValue(MailMergeContext.CONTEXT_SCREENING_ORGANISATIE, screeningOrganisatie);
		context.putValue(MailMergeContext.CONTEXT_MAMMA_CE, SpringBeanProvider.getInstance().getBean(ClientService.class).bepaalCe(client));
		context.setIntakeAfspraak(wrapper.getIntakeAfspraak());
		context.setBmhkLaboratorium(bmhkLaboratorium);

		CervixBrief brief = new CervixBrief();
		context.setBrief(brief);
		brief.setMonster(cervixUitnodiging.getMonster());

		CervixUitstrijkje cervixUitstrijkje = (CervixUitstrijkje) brief.getMonster();
		if (wrapper.isFromDBBMHKLAB()
			&& wrapper.isMicrobioloog())
		{
			cervixUitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.ONTVANGEN);
		}
		else
		{
			cervixUitstrijkje.setUitstrijkjeStatus(CervixUitstrijkjeStatus.BEOORDEELD_DOOR_CYTOLOGIE);
		}

		return context;
	}

	protected abstract List<MergeFieldTestType> getMergeTypes();

}
