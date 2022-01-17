package nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.overdrachtpersoonsgegevens;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.algemeen.OverdrachtPersoonsgegevensService;
import nl.rivm.screenit.main.util.BriefOmschrijvingUtil;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.AjaxDownload;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.popup.DocumentVervangenPanel;
import nl.rivm.screenit.model.AanvraagBriefStatus;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.algemeen.AlgemeneBrief;
import nl.rivm.screenit.model.algemeen.OverdrachtPersoonsgegevens;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.FileService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.upload.FileUpload;
import org.apache.wicket.markup.html.form.upload.FileUploadField;
import org.apache.wicket.markup.html.link.DownloadLink;
import org.apache.wicket.markup.html.list.ListItem;
import org.apache.wicket.markup.html.list.ListView;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.util.ListModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.util.resource.AbstractResourceStreamWriter;
import org.apache.wicket.util.resource.IResourceStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class OverdrachtGegevensAanvraagPopupPanel extends GenericPanel<OverdrachtPersoonsgegevens>
{
	private static final Logger LOG = LoggerFactory.getLogger(OverdrachtGegevensAanvraagPopupPanel.class);

	private static final long serialVersionUID = 1L;

	@SpringBean
	private OverdrachtPersoonsgegevensService overdrachtPersoonsgegevensService;

	@SpringBean
	private FileService fileService;

	@SpringBean
	private HibernateService hibernateService;

	private IModel<List<FileUpload>> files = new ListModel<>();

	private Form<OverdrachtPersoonsgegevens> form;

	private WebMarkupContainer uploadContainer;

	private WebMarkupContainer documentVervangenPanel;

	private BootstrapDialog dialog;

	protected OverdrachtGegevensAanvraagPopupPanel(String id, IModel<OverdrachtPersoonsgegevens> model)
	{
		super(id, new CompoundPropertyModel<>(model));
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		dialog = new BootstrapDialog("dialog");
		add(dialog);
		form = new ScreenitForm<>("form", getModel());
		form.setMultiPart(true);
		add(form);

		addVerstuurdeBrieven();
		addUploadContainer();
		addBvoKeuzeCheckboxes();
		addVervangenPanel();
		addButtons();
	}

	private void addVerstuurdeBrieven()
	{
		List<AlgemeneBrief> verstuurdeBrieven = getVerstuurdeBrieven();
		form.add(new ListView<>("brievenLijst", BriefOmschrijvingUtil.getBrievenOmschrijvingen(verstuurdeBrieven, this::getString))
		{
			@Override
			protected void populateItem(ListItem<String> item)
			{
				item.add(new Label("brief", Model.of(item.getModelObject())));
			}
		});
	}

	private List<AlgemeneBrief> getVerstuurdeBrieven()
	{
		ArrayList<AlgemeneBrief> brieven = new ArrayList<>();
		brieven.add(getModelObject().getVerstuurdeAanvraagbrief());
		if (getModelObject().getGeenHandtekeningBrief() != null)
		{
			brieven.add(getModelObject().getGeenHandtekeningBrief());
		}
		return brieven;
	}

	private boolean briefOntvangen()
	{
		return getModelObject().getOntvangenAanvraagbrief() != null;
	}

	private void addUploadContainer()
	{
		uploadContainer = new WebMarkupContainer("uploadContainer");
		uploadContainer.add(new FileUploadField("fileUpload", files).add(new FileValidator(FileType.PDF)).setRequired(true));
		uploadContainer.setVisible(!briefOntvangen());
		uploadContainer.setOutputMarkupId(true);
		uploadContainer.setOutputMarkupPlaceholderTag(true);
		form.add(uploadContainer);
	}

	private void addBvoKeuzeCheckboxes()
	{
		form.add(ComponentHelper.newCheckBox("bkGegevens").setEnabled(getModelObject().getStatus() != AanvraagBriefStatus.VERWERKT));
		form.add(ComponentHelper.newCheckBox("bkBeelden").setEnabled(getModelObject().getStatus() != AanvraagBriefStatus.VERWERKT));
		form.add(ComponentHelper.newCheckBox("bmhkGegevens").setEnabled(getModelObject().getStatus() != AanvraagBriefStatus.VERWERKT));
		form.add(ComponentHelper.newCheckBox("dkGegevens").setEnabled(getModelObject().getStatus() != AanvraagBriefStatus.VERWERKT));
	}

	private void addVervangenPanel()
	{
		documentVervangenPanel = new DocumentVervangenPanel("documentVervangen")
		{
			@Override
			protected void vervangDocument(UploadDocument uploadDocument, AjaxRequestTarget target)
			{
				try
				{
					overdrachtPersoonsgegevensService.slaOntvangenFormulierOp(getModelObject(), uploadDocument, ScreenitSession.get().getLoggedInAccount());
					info(getString("info.vervangendocument"));
					close(target);
				}
				catch (IOException e)
				{
					LOG.error("Fout bij vervangen document aanvraag persoonsgegevens: ", e);
					error(getString("error.onbekend"));
				}
			}
		};
		documentVervangenPanel.setVisible(false);
		documentVervangenPanel.setOutputMarkupId(true);
		documentVervangenPanel.setOutputMarkupPlaceholderTag(true);
		form.add(documentVervangenPanel);
	}

	private void addButtons()
	{
		addDownloadAanvraagformulierKnop();
		addGeenHandtekeningKnop();
		addVervangKnop();
		addAfrondenKnop();
		addDowndloadDataKnop();
		addOpslaanKnop();
	}

	private void addAfrondenKnop()
	{
		form.add(new ConfirmingIndicatingAjaxLink<Void>("afronden", dialog, "confirm.afronden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				OverdrachtPersoonsgegevens overdrachtPersoonsgegevens = OverdrachtGegevensAanvraagPopupPanel.this.getModelObject();
				overdrachtPersoonsgegevensService.afronden(overdrachtPersoonsgegevens);
				info(getString("info.afgerond"));
				close(target);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(OverdrachtGegevensAanvraagPopupPanel.this.getModelObject().getStatus() == AanvraagBriefStatus.BRIEF_ONTVANGEN
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS, Actie.INZIEN));
			}
		});
	}

	private void addDownloadAanvraagformulierKnop()
	{
		if (briefOntvangen())
		{
			IModel<UploadDocument> upload = ModelUtil.sModel(getModelObject().getOntvangenAanvraagbrief());
			form.add(new DownloadLink("downloadFormulier", new LoadableDetachableModel<>()
			{
				@Override
				protected File load()
				{
					return fileService.load(upload.getObject());
				}
			}, "Aanvraagformulier overdracht persoonsgegevens met Handtekening." + FilenameUtils.getExtension(upload.getObject().getNaam())));
		}
		else
		{
			EmptyPanel downloadFormulier = new EmptyPanel("downloadFormulier");
			downloadFormulier.setVisible(false);
			form.add(downloadFormulier);
		}
	}

	private void addGeenHandtekeningKnop()
	{
		form.add(new AjaxLink<Void>("geenHandtekening")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				OverdrachtPersoonsgegevens overdracht = OverdrachtGegevensAanvraagPopupPanel.this.getModelObject();
				overdrachtPersoonsgegevensService.verstuurGeenHandtekeningBrief(overdracht, ScreenitSession.get().getLoggedInAccount());
				info(getString("info.geenhandtekening.overdrachtgegevens"));
				close(target);
			}

		}.setVisible(getModelObject().getGeenHandtekeningBrief() == null && !briefOntvangen()));
	}

	private void addVervangKnop()
	{
		form.add(new AjaxLink<Void>("vervangen")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				documentVervangenPanel.setVisible(true);
				target.add(documentVervangenPanel);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(OverdrachtGegevensAanvraagPopupPanel.this.getModelObject().getOntvangenAanvraagbrief() != null
					&& ScreenitSession.get().checkPermission(Recht.VERVANGEN_DOCUMENTEN, Actie.AANPASSEN));
			}
		});
	}

	private void addOpslaanKnop()
	{
		form.add(new AjaxSubmitLink("opslaan")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				valideerInvoer();
				if (!OverdrachtGegevensAanvraagPopupPanel.this.hasErrorMessage())
				{
					saveOverdracht();
					close(target);
				}
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(OverdrachtGegevensAanvraagPopupPanel.this.getModelObject().getStatus() != AanvraagBriefStatus.VERWERKT);
			}
		});
	}

	private void valideerInvoer()
	{
		if (uploadContainer.isVisible() && files.getObject().size() != 1)
		{
			error(getString("error.een.bestand.uploaden.overdrachtgegevens"));
		}

		if (!keuzeGeselecteerd())
		{
			error(getString("error.minimaal.een.keuze"));
		}
	}

	private boolean keuzeGeselecteerd()
	{
		OverdrachtPersoonsgegevens overdracht = getModelObject();
		return Boolean.TRUE.equals(overdracht.getBkGegevens()) || Boolean.TRUE.equals(overdracht.getBkBeelden())
			|| Boolean.TRUE.equals(overdracht.getBmhkGegevens()) || Boolean.TRUE.equals(overdracht.getDkGegevens());
	}

	private void saveOverdracht()
	{
		try
		{
			if (uploadContainer.isVisible())
			{
				UploadDocument uploadDocument = maakUploadDocument();
				overdrachtPersoonsgegevensService.slaOntvangenFormulierOp(getModelObject(), uploadDocument, ScreenitSession.get().getLoggedInAccount());
			}
			else
			{
				hibernateService.saveOrUpdate(getModelObject());
			}
			info(getString("info.overdrachtgegevens.opgeslagen"));
		}
		catch (Exception e)
		{
			LOG.error("Fout bij uploaden van brief aanvraag persoonsgegevens: ", e);
			error(getString("error.onbekend"));
		}
	}

	private UploadDocument maakUploadDocument() throws Exception
	{
		FileUpload upload = files.getObject().get(0);
		UploadDocument document = new UploadDocument();
		document.setActief(Boolean.TRUE);
		document.setContentType(upload.getContentType());
		document.setFile(upload.writeToTempFile());
		document.setNaam(upload.getClientFileName());
		return document;
	}

	@Override
	protected void onDetach()
	{
		ModelUtil.nullSafeDetach(files);
		super.onDetach();
	}

	private void addDowndloadDataKnop()
	{
		final AjaxDownload download = new AjaxDownload()
		{
			@Override
			protected IResourceStream getResourceStream()
			{
				return createResourceStream();
			}

			@Override
			protected String getFileName()
			{
				return "clientgegevens.xlsx";
			}

			private IResourceStream createResourceStream()
			{
				AbstractResourceStreamWriter rstream = new AbstractResourceStreamWriter()
				{

					@Override
					public void write(OutputStream output)
					{
						ByteArrayOutputStream outputStream = null;
						InputStream inputStream = null;
						try
						{
							outputStream = new ByteArrayOutputStream();
							overdrachtPersoonsgegevensService.createDataDump(OverdrachtGegevensAanvraagPopupPanel.this.getModelObject(), outputStream,
								ScreenitSession.get().getLoggedInInstellingGebruiker());

							inputStream = new ByteArrayInputStream(outputStream.toByteArray());
							IOUtils.copy(inputStream, output);
						}
						catch (IOException e)
						{
							LOG.error("Fout bij het aanmaken van XLSX: " + e.getMessage(), e);
						}
						finally
						{
							close(outputStream);
							close(output);
							close(inputStream);
						}
					}

					private void close(Closeable closable)
					{
						if (closable != null)
						{
							try
							{
								closable.close();
							}
							catch (IOException e)
							{
								LOG.error("Fout bij het sluiten van stream: " + e.getMessage(), e);
							}
						}
					}
				};

				return rstream;
			}
		};
		form.add(download);

		form.add(new ScreenitIndicatingAjaxSubmitLink("download", form)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);
				hibernateService.saveOrUpdate(getModelObject());
				download.initiate(target);
			}

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				setVisible(OverdrachtGegevensAanvraagPopupPanel.this.getModelObject().getStatus() == AanvraagBriefStatus.BRIEF_ONTVANGEN
					&& ScreenitSession.get().checkPermission(Recht.GEBRUIKER_DOWNLOAD_OVERDRACHT_PERSOONSGEGEVENS, Actie.INZIEN));
			}

		});
	}

	public abstract void close(AjaxRequestTarget target);

}
