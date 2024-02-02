package nl.rivm.screenit.main.service.impl;

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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import javax.persistence.Temporal;

import nl.rivm.screenit.main.dao.ScreenitFormulierDao;
import nl.rivm.screenit.main.model.formulieren.BeanAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.main.model.formulieren.BeanAntwoordVraagDefinitieImpl;
import nl.rivm.screenit.main.model.formulieren.DSBeanAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.main.model.formulieren.UnitOption;
import nl.rivm.screenit.main.service.FormulierService;
import nl.rivm.screenit.model.formulieren.IdentifierElement;
import nl.rivm.screenit.model.formulieren.PalgaNumber;
import nl.rivm.screenit.model.formulieren.PalgaNumberAntwoordDefintie;
import nl.rivm.screenit.model.formulieren.ScreenitFormulierInstantie;
import nl.rivm.screenit.model.formulieren.SimpleAntwoordKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.model.formulieren.SimpleKeuzeVraagDefinitieImpl;
import nl.rivm.screenit.model.formulieren.SimpleVraagDefinitieImpl;
import nl.rivm.screenit.model.formulieren.TypeFormulier;
import nl.rivm.screenit.model.verslag.DSValueSet;
import nl.rivm.screenit.model.verslag.Quantity;
import nl.rivm.screenit.model.verslag.VraagElement;
import nl.rivm.screenit.model.verslag.VraagElementUnit;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.topicuszorg.formulieren2.api.definitie.VraagDefinitie;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElement;
import nl.topicuszorg.formulieren2.api.instantie.FormulierElementContainer;
import nl.topicuszorg.formulieren2.api.instantie.FormulierInstantie;
import nl.topicuszorg.formulieren2.api.instantie.VraagInstantie;
import nl.topicuszorg.formulieren2.api.instantie.containers.SamengesteldeVraagBlok;
import nl.topicuszorg.formulieren2.api.rendering.AntwoordRenderType;
import nl.topicuszorg.formulieren2.api.service.IFormulierEditFactory;
import nl.topicuszorg.formulieren2.beanantwoord.BeanAntwoordVraagInstantie;
import nl.topicuszorg.formulieren2.beanantwoord.PropertyPathLocation;
import nl.topicuszorg.formulieren2.excel.ExcelImporter;
import nl.topicuszorg.formulieren2.excel.FormulierMapping;
import nl.topicuszorg.formulieren2.excel.VraagProvider;
import nl.topicuszorg.formulieren2.persistence.definitie.BooleanAntwoordDefinitie;
import nl.topicuszorg.formulieren2.persistence.definitie.IntegerAntwoordDefintie;
import nl.topicuszorg.formulieren2.persistence.definitie.StringAntwoordDefinitie;
import nl.topicuszorg.formulieren2.persistence.instantie.FormulierInstantieImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VerplichtingImpl;
import nl.topicuszorg.formulieren2.persistence.instantie.VraagInstantieImpl;
import nl.topicuszorg.hibernate.object.model.HibernateObject;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
public class FormulierServiceImpl implements FormulierService
{

	@Autowired
	private IFormulierEditFactory formulierEditFactory;

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	private ScreenitFormulierDao screenitFormulierDao;

	@Autowired
	private ICurrentDateSupplier currentDateSupplier;

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<ScreenitFormulierInstantie> importFormulier(TypeFormulier typeFormulier, File file, String domein) throws InvalidFormatException, IOException,
		ClassNotFoundException
	{
		return importFormulier(typeFormulier, file, domein, false);
	}

	@Override
	@Transactional(propagation = Propagation.REQUIRED)
	public List<ScreenitFormulierInstantie> importFormulier(TypeFormulier typeFormulier, File file, final String domein, boolean replace) throws InvalidFormatException,
		IOException, ClassNotFoundException
	{
		if (replace)
		{
			ScreenitFormulierInstantie screenitFormulierInstantie = null;
			while ((screenitFormulierInstantie = screenitFormulierDao.getFormulierInstatie(typeFormulier)) != null)
			{
				hibernateService.delete(screenitFormulierInstantie);
			}
		}

		final List<BeanAntwoordVraagDefinitieImpl<?>> vraagDefinities = new ArrayList<>();

		createVraagDefinties(typeFormulier.getRootEntityClass(), vraagDefinities, "");
		try (InputStream inputStream = new FileInputStream(file))
		{
			importVraagDefinties(inputStream, domein);
		}

		try (InputStream inputStream = new FileInputStream(file))
		{
			List<FormulierInstantie<?, ?>> formulierInstanties = new ExcelImporter()
			{

				@Override
				protected FormulierElement addVraag(IFormulierEditFactory formulierEditFactory, VraagProvider vraagProvider, FormulierInstantie current,
					Stack<ContainerStack> containers, Row row, FormulierElementContainer<?> container, Map<Row, VraagInstantie<?>> teImporterenActies)
				{
					FormulierElement formulierElement = super.addVraag(formulierEditFactory, vraagProvider, current, containers, row, container, teImporterenActies);
					if (formulierElement instanceof SamengesteldeVraagBlok)
					{
						SamengesteldeVraagBlok<?> samengesteldeVraagBlok = (SamengesteldeVraagBlok<?>) formulierElement;

						Cell actieCell = row.getCell(FormulierMapping.ACTIE.getColumn());
						if (actieCell != null && StringUtils.isNotBlank(actieCell.getStringCellValue()))
						{
							for (FormulierElement valueElement : samengesteldeVraagBlok.getElementen())
							{
								if (valueElement instanceof BeanAntwoordVraagInstantie)
								{
									BeanAntwoordVraagInstantie<?> valueVraagInstantie = (BeanAntwoordVraagInstantie<?>) valueElement;
									String propertyPath = valueVraagInstantie.getPropertyPath();
									if (propertyPath != null && propertyPath.endsWith(".value"))
									{
										teImporterenActies.put(row, valueVraagInstantie);
										break;
									}
								}
							}
						}
					}
					else if (formulierElement instanceof VraagInstantieImpl)
					{
						VraagInstantieImpl instantie = (VraagInstantieImpl) formulierElement;
						instantie.setMaxLength(4096);
					}
					return formulierElement;
				}

			}.importExcelFormulier(inputStream, formulierEditFactory, new VraagProvider()
			{

				@Override
				public VraagDefinitie<?> findVraagDefinitie(String identifier)
				{
					for (BeanAntwoordVraagDefinitieImpl<?> vraagDefinitie : vraagDefinities)
					{
						if (vraagDefinitie.getCode().equals(identifier))
						{
							return vraagDefinitie;
						}
					}

					return screenitFormulierDao.findSimpleVraagDefinitieImplByIdentifier(identifier, domein);
				}

				@Override
				public boolean isVraag(String doelElement, VraagInstantie vraagInstantie)
				{
					if (vraagInstantie.getVraagDefinitie() instanceof IdentifierElement)
					{
						IdentifierElement identifierElement = (IdentifierElement) vraagInstantie.getVraagDefinitie();
						return identifierElement.getIdentifier() != null && identifierElement.getIdentifier().equals(doelElement);
					}

					return false;
				}

			});
			hibernateService.saveOrUpdateAll(vraagDefinities);

			List<ScreenitFormulierInstantie> formulieren = new ArrayList<>();
			for (FormulierInstantie formulierInstantie : formulierInstanties)
			{
				ScreenitFormulierInstantie screenitFormulierInstantie = (ScreenitFormulierInstantie) formulierInstantie;
				screenitFormulierInstantie.setCreatieDatum(currentDateSupplier.getDate());
				screenitFormulierInstantie.setTypeFormulier(typeFormulier);

				hibernateService.saveOrUpdate(screenitFormulierInstantie);
				formulieren.add(screenitFormulierInstantie);
			}

			return formulieren;
		}
	}

	private void importVraagDefinties(InputStream inputStream, String domein) throws InvalidFormatException, IOException, ClassNotFoundException
	{
		try (Workbook workbook = WorkbookFactory.create(new PushbackInputStream(inputStream)))
		{
			Sheet sheet = workbook.getSheet("Vragen");
			if (sheet == null)
			{
				throw new IllegalStateException("Geen sheet genaamd 'Vragen' gevonden");
			}

			int index = 1;
			boolean continueReading = true;
			while (continueReading)
			{
				Row row = sheet.getRow(index);

				if (row != null)
				{
					Cell cell3 = row.getCell(3);
					if (isCellNotEmpty(cell3))
					{
						String identifier = cell3.getStringCellValue();
						VraagDefinitie vraag = screenitFormulierDao.findSimpleVraagDefinitieImplByIdentifier(identifier, domein);
						if (vraag == null)
						{

							if (isCellNotEmpty(row.getCell(4)) && isCellNotEmpty(row.getCell(5)))
							{
								SimpleAntwoordKeuzeVraagDefinitieImpl nieuweVraag = new SimpleAntwoordKeuzeVraagDefinitieImpl<>();
								nieuweVraag.setRenderType(AntwoordRenderType.valueOf(row.getCell(4).getStringCellValue()));
								if (row.getCell(1) != null)
								{
									nieuweVraag.setAanvullendeInformatie(row.getCell(1).getStringCellValue());
								}

								maakMogelijkheden(row, nieuweVraag);

								if (isVerplichteVraag(row))
								{
									nieuweVraag.setVerplichting(new VerplichtingImpl());
								}
								vraag = nieuweVraag;
							}
							else if (isCellNotEmpty(row.getCell(4)))
							{

								SimpleKeuzeVraagDefinitieImpl nieuweVraag = new SimpleKeuzeVraagDefinitieImpl<>();
								nieuweVraag.setRenderType(AntwoordRenderType.valueOf(row.getCell(4).getStringCellValue()));

								if (isVerplichteVraag(row))
								{
									nieuweVraag.setVerplichting(new VerplichtingImpl());
								}

								vraag = nieuweVraag;
							}
							else
							{

								SimpleVraagDefinitieImpl nieuweVraag = new SimpleVraagDefinitieImpl<>();

								if (isVerplichteVraag(row))
								{
									nieuweVraag.setVerplichting(new VerplichtingImpl());
								}

								vraag = nieuweVraag;
							}
							if (row.getCell(8) != null && row.getCell(8).getCellType() == CellType.STRING)
							{
								vraag.setExpressieVariabele(row.getCell(8).getStringCellValue());
							}
							vraag.setVraag(row.getCell(0).getStringCellValue());
							vraag.setAntwoordTypeClass(Class.forName(row.getCell(2).getStringCellValue()));
							if (vraag instanceof IdentifierElement)
							{
								((IdentifierElement) vraag).setIdentifier(identifier);
							}
						}
						else
						{
							if (vraag instanceof SimpleAntwoordKeuzeVraagDefinitieImpl)
							{
								SimpleAntwoordKeuzeVraagDefinitieImpl simpleAntwoordKeuzeVraagDefinitie = (SimpleAntwoordKeuzeVraagDefinitieImpl) vraag;
								if (isVerplichteVraag(row))
								{
									simpleAntwoordKeuzeVraagDefinitie.setVerplichting(new VerplichtingImpl());
								}
								else
								{
									simpleAntwoordKeuzeVraagDefinitie.setVerplichting(null);
								}

								if (CollectionUtils.isNotEmpty(simpleAntwoordKeuzeVraagDefinitie.getMogelijkeAntwoorden()))
								{
									hibernateService.deleteAll(simpleAntwoordKeuzeVraagDefinitie.getMogelijkeAntwoorden());
								}
								simpleAntwoordKeuzeVraagDefinitie.getMogelijkeAntwoorden().clear();
								maakMogelijkheden(row, simpleAntwoordKeuzeVraagDefinitie);
							}
							else if (vraag instanceof SimpleKeuzeVraagDefinitieImpl)
							{
								SimpleKeuzeVraagDefinitieImpl simpleKeuzeVraagDefinitie = (SimpleKeuzeVraagDefinitieImpl) vraag;
								if (isVerplichteVraag(row))
								{
									simpleKeuzeVraagDefinitie.setVerplichting(new VerplichtingImpl());
								}
								else
								{
									simpleKeuzeVraagDefinitie.setVerplichting(null);
								}
							}
							else if (vraag instanceof SimpleVraagDefinitieImpl)
							{
								SimpleVraagDefinitieImpl simpleVraagDefinitie = (SimpleVraagDefinitieImpl) vraag;
								if (isVerplichteVraag(row))
								{
									simpleVraagDefinitie.setVerplichting(new VerplichtingImpl());
								}
								else
								{
									simpleVraagDefinitie.setVerplichting(null);
								}
							}
							if (row.getCell(8) != null && row.getCell(8).getCellType() == CellType.STRING)
							{
								vraag.setExpressieVariabele(row.getCell(8).getStringCellValue());
							}

							vraag.setVraag(row.getCell(0).getStringCellValue());
							vraag.setAntwoordTypeClass(Class.forName(row.getCell(2).getStringCellValue()));
						}
						if (vraag instanceof IdentifierElement)
						{
							((IdentifierElement) vraag).setDomein(domein);
						}
						hibernateService.saveOrUpdate((HibernateObject) vraag);
					}
				}
				else
				{
					break;
				}
				index++;

			}
		}
	}

	private void maakMogelijkheden(Row row, SimpleAntwoordKeuzeVraagDefinitieImpl vraag)
	{
		String[] stringValues = row.getCell(5).getStringCellValue().split(";");
		String[] values = row.getCell(6).getStringCellValue().split(";");

		for (int i = 0; i < stringValues.length; i++)
		{
			if (row.getCell(2).getStringCellValue().equals(Integer.class.getName()))
			{
				IntegerAntwoordDefintie antwoordDefintie = new IntegerAntwoordDefintie();
				antwoordDefintie.setAntwoordKeuzeVraagDefinitie(vraag);
				antwoordDefintie.setAntwoordString(stringValues[i]);
				antwoordDefintie.setAntwoordValue(Integer.parseInt(values[i]));
				vraag.getMogelijkeAntwoorden().add(antwoordDefintie);
			}
			else if (row.getCell(2).getStringCellValue().equals(Boolean.class.getName()))
			{
				BooleanAntwoordDefinitie antwoordDefinitie = new BooleanAntwoordDefinitie();
				antwoordDefinitie.setAntwoordKeuzeVraagDefinitie(vraag);
				antwoordDefinitie.setAntwoordString(stringValues[i]);
				antwoordDefinitie.setAntwoordValue(Boolean.parseBoolean(values[i]));
				vraag.getMogelijkeAntwoorden().add(antwoordDefinitie);
			}
			else if (row.getCell(2).getStringCellValue().equals(String.class.getName()))
			{
				StringAntwoordDefinitie antwoordDefinitie = new StringAntwoordDefinitie();
				antwoordDefinitie.setAntwoordKeuzeVraagDefinitie(vraag);
				antwoordDefinitie.setAntwoordString(stringValues[i]);
				antwoordDefinitie.setAntwoordValue(values[i]);
				vraag.getMogelijkeAntwoorden().add(antwoordDefinitie);
			}
			else if (row.getCell(2).getStringCellValue().equals(PalgaNumber.class.getName()))
			{
				PalgaNumberAntwoordDefintie palgaLabelAntwoord = new PalgaNumberAntwoordDefintie();
				palgaLabelAntwoord.setAntwoordKeuzeVraagDefinitie(vraag);
				palgaLabelAntwoord.setAntwoordValue(new PalgaNumber(Integer.parseInt(values[i]), stringValues[i]));
				vraag.getMogelijkeAntwoorden().add(palgaLabelAntwoord);
			}
		}
	}

	private boolean isCellNotEmpty(Cell cell)
	{
		return cell != null && StringUtils.isNotBlank(cell.getStringCellValue());
	}

	private boolean isVerplichteVraag(Row row)
	{
		boolean bool = false;
		if (row.getCell(7) != null)
		{
			if (row.getCell(7).getCellType() == CellType.STRING)
			{
				String value = row.getCell(7).getStringCellValue();
				if (StringUtils.equalsIgnoreCase("TRUE", value) || StringUtils.equalsIgnoreCase("WAAR", value) || StringUtils.equalsIgnoreCase("JA", value))
				{
					bool = true;
				}
			}
			else if (row.getCell(7).getCellType() == CellType.BOOLEAN)
			{
				bool = row.getCell(7).getBooleanCellValue();
			}
		}
		return bool;
	}

	private void createVraagDefinties(Class<?> clazz, List<BeanAntwoordVraagDefinitieImpl<?>> vraagDefinities, String propertyPath)
	{
		for (Field field : getAllFields(clazz))
		{
			VraagElement vraagElement = field.getAnnotation(VraagElement.class);
			Class<?> fieldType = field.getType();
			if (vraagElement != null && vraagElement.useInFormulier())
			{

				if (vraagElement.isReference())
				{
					if (fieldType.isAssignableFrom(List.class))
					{
						Type type = field.getGenericType();
						if (type instanceof ParameterizedType)
						{
							ParameterizedType ptype = (ParameterizedType) type;
							createVraagDefinties((Class) ptype.getActualTypeArguments()[0], vraagDefinities, propertyPath + field.getName() + "[$i].");
						}
					}
					else
					{
						createVraagDefinties(fieldType, vraagDefinities, propertyPath + field.getName() + ".");
					}
				}
				else
				{
					DSValueSet valueSet = field.getAnnotation(DSValueSet.class);

					if (valueSet != null || fieldType.equals(Boolean.class))
					{

						BeanAntwoordKeuzeVraagDefinitieImpl beanAntwoordKeuzeVraagDefinitieImpl;
						if (fieldType.equals(Boolean.class))
						{
							beanAntwoordKeuzeVraagDefinitieImpl = new BeanAntwoordKeuzeVraagDefinitieImpl<>();
						}
						else
						{
							beanAntwoordKeuzeVraagDefinitieImpl = new DSBeanAntwoordKeuzeVraagDefinitieImpl();
							((DSBeanAntwoordKeuzeVraagDefinitieImpl) beanAntwoordKeuzeVraagDefinitieImpl).setDsValueField(clazz.getName() + "|" + field.getName());
						}

						beanAntwoordKeuzeVraagDefinitieImpl.setAntwoordTypeClass(fieldType);
						beanAntwoordKeuzeVraagDefinitieImpl.setPropertyPath(propertyPath + field.getName());
						beanAntwoordKeuzeVraagDefinitieImpl.setVraag(vraagElement.displayName());
						beanAntwoordKeuzeVraagDefinitieImpl.setAanvullendeInformatie(vraagElement.extraTekst());
						beanAntwoordKeuzeVraagDefinitieImpl.setRenderType(AntwoordRenderType.DROPDOWN);
						beanAntwoordKeuzeVraagDefinitieImpl.setPropertyPathLocation(PropertyPathLocation.DEFINTIE);

						if (vraagElement.isVerplicht())
						{
							beanAntwoordKeuzeVraagDefinitieImpl.setVerplichting(new VerplichtingImpl());
						}

						beanAntwoordKeuzeVraagDefinitieImpl.setMeervoudig(fieldType.isAssignableFrom(List.class));
						beanAntwoordKeuzeVraagDefinitieImpl.setCode(vraagElement.code());
						beanAntwoordKeuzeVraagDefinitieImpl.setExpressieVariabele(getExpressieVariabele(vraagElement.displayName()));
						vraagDefinities.add(beanAntwoordKeuzeVraagDefinitieImpl);
					}
					else
					{
						BeanAntwoordVraagDefinitieImpl beanAntwoordVraagDefinitieImpl = new BeanAntwoordVraagDefinitieImpl<>();
						beanAntwoordVraagDefinitieImpl.setAntwoordTypeClass(fieldType);
						if (Date.class.isAssignableFrom(fieldType))
						{
							Temporal dateTemporal = field.getAnnotation(Temporal.class);
							if (dateTemporal != null && dateTemporal.value() != null)
							{
								beanAntwoordVraagDefinitieImpl.setMetaInfoOpAntwoordTypeClass(dateTemporal.value().toString());
							}

						}
						if (Quantity.class.isAssignableFrom(fieldType))
						{
							beanAntwoordVraagDefinitieImpl.setPropertyPathLocation(PropertyPathLocation.INSTANTIE);
						}
						else
						{
							beanAntwoordVraagDefinitieImpl.setPropertyPathLocation(PropertyPathLocation.DEFINTIE);
						}
						beanAntwoordVraagDefinitieImpl.setPropertyPath(propertyPath + field.getName());
						beanAntwoordVraagDefinitieImpl.setVraag(vraagElement.displayName());
						beanAntwoordVraagDefinitieImpl.setCode(vraagElement.code());
						beanAntwoordVraagDefinitieImpl.setAanvullendeInformatie(vraagElement.extraTekst());

						if (vraagElement.unit() != null)
						{
							for (VraagElementUnit vraagElementUnit : vraagElement.unit())
							{
								UnitOption unitOption = new UnitOption();
								unitOption.setUnit(vraagElementUnit.unit());
								if (StringUtils.isNotBlank(vraagElementUnit.max()))
								{
									unitOption.setMax(Double.parseDouble(vraagElementUnit.max()));
								}

								if (StringUtils.isNotBlank(vraagElementUnit.min()))
								{
									unitOption.setMin(Double.parseDouble(vraagElementUnit.min()));
								}

								beanAntwoordVraagDefinitieImpl.getUnitOptions().add(unitOption);
							}
						}

						if (vraagElement.isVerplicht())
						{
							beanAntwoordVraagDefinitieImpl.setVerplichting(new VerplichtingImpl());
						}
						beanAntwoordVraagDefinitieImpl.setExpressieVariabele(getExpressieVariabele(vraagElement.displayName()));
						vraagDefinities.add(beanAntwoordVraagDefinitieImpl);
					}
				}
			}

		}
	}

	private String getExpressieVariabele(String name)
	{
		if (StringUtils.containsAny(name, ":/+*&-+=<>()\\."))
		{
			return name.replaceAll("/|\\+|\\*|&|-|=|<|>|\\(|\\)|\\s|\\.|:", "_").toLowerCase();
		}
		return null;
	}

	private List<Field> getAllFields(Class<?> clazz)
	{
		List<Field> fields = new ArrayList<Field>();
		fields.addAll(Arrays.asList(clazz.getDeclaredFields()));

		if (clazz.getSuperclass() != null)
		{
			fields.addAll(getAllFields(clazz.getSuperclass()));
		}

		return fields;
	}

	public void setFormulierEditFactory(IFormulierEditFactory formulierEditFactory)
	{
		this.formulierEditFactory = formulierEditFactory;
	}

	@Override
	public <T> VraagInstantieImpl<T> findVraagInstantieByIdentifier(FormulierInstantieImpl formulierInstantie, String identifier)
	{
		return screenitFormulierDao.findVraagInstantieByIdentifier(formulierInstantie, identifier);
	}

	@Override
	public ScreenitFormulierInstantie getFormulierInstatie(TypeFormulier typeFormulier)
	{
		return screenitFormulierDao.getFormulierInstatie(typeFormulier);
	}
}
